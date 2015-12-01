package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Transforms the coroutine body into a two operand assignment form with restricted
 *  control flow that contains only try-catch statements, while loops, do-while loops,
 *  if-statements, value and variable declarations, nested blocks and function calls.
 *
 *  Newly synthesized variables get mangled fresh names, and existing variable names are
 *  preserved.
 */
trait TwoOperandAssignmentTransform[C <: Context] {
  self: Analyzer[C] =>

  val c: C

  import c.universe._

  object NestedContextValidator extends Traverser {
    override def traverse(tree: Tree): Unit = tree match {
      case q"coroutines.this.`package`.coroutine[$_]($_)" =>
        // no need to check further, this is checked in a different expansion
      case q"coroutines.this.`package`.yieldval[$_]($_)" =>
        c.abort(
          tree.pos,
          "The yieldval statement only be invoked directly inside the coroutine. " +
          "Nested classes, functions or for-comprehensions, should either use the " +
          "call statement or declare another coroutine.")
      case q"coroutines.this.`package`.yieldto[$_]($_)" =>
        c.abort(
          tree.pos,
          "The yieldto statement only be invoked directly inside the coroutine. " +
          "Nested classes, functions or for-comprehensions, should either use the " +
          "call statement or declare another coroutine.")
      case q"coroutines.this.`package`.call($co.apply(..$args))" =>
        // no need to check further, the call macro will validate the coroutine type
      case q"$co.apply(..$args)" if isCoroutineBlueprint(co.tpe) =>
        c.abort(
          tree.pos,
          "Coroutine blueprints can only be invoked directly inside the coroutine. " +
          "Nested classes, functions or for-comprehensions, should either use the " +
          "call statement or declare another coroutine.")
      case _ =>
        super.traverse(tree)
    }
  }

  def disallowCoroutinesIn(tree: Tree): Unit = {
    for (t <- tree) t match {
      case CoroutineOp(t) => c.abort(t.pos, "Coroutines disallowed in:\n$tree.")
      case _ => // fine
    }
  }

  private def tearExpression(tree: Tree)(
    implicit table: Table
  ): (List[Tree], Tree) = tree match {
    case q"$r.$member" =>
      // selection
      val (rdecls, rident) = tearExpression(r)
      val localvarname = TermName(c.freshName())
      val localvartree = q"val $localvarname = $rident.$member"
      (rdecls ++ List(localvartree), q"$localvarname")
    case q"$r.$method(..$args)" =>
      // application
      // TODO: translate boolean && and || to if statements, then regenerate, to adher
      // to the short-circuit evaluation rules
      val (rdecls, rident) = tearExpression(r)
      val (argdecls, argidents) = args.map(tearExpression).unzip
      val localvarname = TermName(c.freshName())
      val localvartree = q"val $localvarname = $rident.$method(..$argidents)"
      (rdecls ++ argdecls.flatten ++ List(localvartree), q"$localvarname")
    case q"$r[..$tpts]" if tpts.length > 0 =>
      // type application
      for (tpt <- tpts) disallowCoroutinesIn(tpt)
      val (rdecls, rident) = tearExpression(r)
      (rdecls, q"$rident[..$tpts]")
    case q"$x(..$args) = $v" =>
      // assignment
      // update
      val (xdecls, xident) = tearExpression(x)
      val (argdecls, argidents) = args.map(tearExpression).unzip
      val (vdecls, vident) = tearExpression(v)
      (xdecls ++ argdecls.flatten ++ vdecls, q"$xident(..$argidents) = $vident")
    case q"return $_" =>
      // return
      c.abort(tree.pos, "The return statement is not allowed inside coroutines.")
    case q"throw $e" =>
      // throw
      val (edecls, eident) = tearExpression(e)
      (edecls, q"throw $eident")
    case q"$x: $tpt" =>
      // ascription
      disallowCoroutinesIn(tpt)
      val (xdecls, xident) = tearExpression(x)
      (xdecls, q"xident: $tpt")
    case q"$x: @$annot" =>
      // annotation
      val (xdecls, xident) = tearExpression(x)
      (xdecls, q"xident: $annot")
    case q"(..$xs)" if xs.length > 1 =>
      // tuples
      val (xsdecls, xsidents) = xs.map(tearExpression).unzip
      (xsdecls.flatten, q"(..$xsidents)")
    case q"if ($cond) $thenbranch else $elsebranch" =>
      // if
      val (conddecls, condident) = tearExpression(cond)
      val (thendecls, thenident) = tearExpression(thenbranch)
      val (elsedecls, elseident) = tearExpression(elsebranch)
      val localvarname = TermName(c.freshName())
      val decls = List(
        q"var $localvarname = null.asInstanceOf[${tree.tpe}]",
        q"""
          ..$conddecls
          if ($condident) {
            ..$thendecls
            $localvarname = $thenident
          } else {
            ..$elsedecls
            $localvarname = $elseident
          }
        """
      )
      (decls, q"$localvarname")
    case q"$x match { case ..$cases }" =>
      // pattern match
      val localvarname = TermName(c.freshName())
      val ncases = for (cq"$pat => $branch" <- cases) yield {
        disallowCoroutinesIn(pat)
        val (branchdecls, branchident) = tearExpression(branch)
        cq"""
          $pat =>
            ..$branchdecls
            $localvarname = $branchident
        """
      }
      val (xdecls, xident) = tearExpression(x)
      val nmatch = q"""
        var $localvarname = null.asInstanceOf[${tree.tpe.widen}]

        ..$xdecls

        $x match {
          case ..$ncases
        }
      """
      (List(nmatch), q"$localvarname")
    case q"(..$params) => $body" =>
      // function
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"{ case ..$cases }" =>
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"while ($cond) $body" =>
      // while
      val (xdecls, xident) = tearExpression(cond)
      val localvarname = TermName(c.freshName())
      val decls = if (xdecls != Nil) {
        xdecls ++ List(
          q"var $localvarname = $xident",
          q"""
            while ($localvarname) {
              ${transform(body)}

              ..$xdecls
              localvarname = $xident
            }
          """)
      } else List(q"""
        while ($cond) {
          ${transform(body)}
        }
      """)
      (decls, q"()")
    case q"do $body while ($cond)" =>
      // do-while
      val (xdecls, xident) = tearExpression(cond)
      val localvarname = TermName(c.freshName())
      val decls = if (xdecls != Nil) xdecls ++ List(
        q"var $localvarname = $xident",
        q"""
          do {
            ${transform(body)}

            ..$xdecls
            localvarname = $xident
          } while ($localvarname)
        """
      ) else List(q"""
        do {
          ${transform(body)}
        } while ($cond)
      """)
      (decls, q"()")
    case q"for (..$enums) $body" =>
      // for loop
      for (e <- enums) NestedContextValidator.traverse(e)
      NestedContextValidator.traverse(body)
      (Nil, tree)
    case q"for (..$enums) yield $body" =>
      // for-yield loop
      for (e <- enums) NestedContextValidator.traverse(e)
      NestedContextValidator.traverse(body)
      (Nil, tree)
    case q"new { ..$edefs } with ..$bases { $self => ..$stats }" =>
      // new
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case Block(stats, expr) =>
      // block
      val localvarname = TermName(c.freshName())
      val (statdecls, statidents) = stats.map(tearExpression).unzip
      val (exprdecls, exprident) = tearExpression(q"$localvarname = $expr")
      val decls =
        List(q"var $localvarname = null.asInstanceOf[${expr.tpe.widen}]") ++
        statdecls.flatten ++
        exprdecls
      (decls, q"$localvarname")
    case tpt: TypeTree =>
      // type trees
      disallowCoroutinesIn(tpt)
      (Nil, tree)
    case q"$mods val $v: $tpt = $rhs" =>
      // val
      val (rhsdecls, rhsident) = tearExpression(rhs)
      val decls = rhsdecls ++ List(q"$mods val $v: $tpt = $rhsident")
      (decls, q"")
    case q"$mods var $v: $tpt = $rhs" =>
      // var
      val (rhsdecls, rhsident) = tearExpression(rhs)
      val decls = rhsdecls ++ List(q"$mods var $v: $tpt = $rhsident")
      (decls, q"")
    case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
      // method
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"$mods type $tpname[..$tparams] = $tpt" =>
      // type
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"""
      $_ class $_[..$_] $_(...$_)
      extends { ..$_ } with ..$_ { $_ => ..$_ }
    """ =>
      // class
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"""
      $_ trait $_[..$_] extends { ..$_ } with ..$_ { $_ => ..$_ }
    """ =>
      // trait
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"""
      $_ object $_ extends { ..$_ } with ..$_ { $_ => ..$_ }
    """ =>
      // object
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case _ =>
      // empty
      // literal
      // identifier
      // super selection
      // this selection
      (Nil, tree)
  }

  private def transform(tree: Tree)(
    implicit table: Table
  ): Tree = tree match {
    case Block(stats, expr) =>
      val (statdecls, statidents) = stats.map(tearExpression).unzip
      val (exprdecls, exprident) = tearExpression(expr)
      q"""
        ..${statdecls.flatten}

        ..$exprdecls
      """
    case t =>
      val (decls, _) = tearExpression(t)
      q"..$decls"
  }

  def transformToTwoOperandForm(body: Tree)(implicit table: Table): Tree = {
    // recursive transform of the body code
    transform(body)
  }
}
