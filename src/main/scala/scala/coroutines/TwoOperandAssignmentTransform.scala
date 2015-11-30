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
 *
 *  Two operand transform also populates the `Table` object with local variable info.
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
          "The yieldval statement can only be called directly inside the coroutine. " +
          "Nested classes and functions must declare a separate nested coroutine to " +
          "yield values.")
      case q"coroutines.this.`package`.yieldto[$_]($_)" =>
        c.abort(
          tree.pos,
          "The yieldto statement can only be called directly inside the coroutine. " +
          "Nested classes and functions must declare a separate nested coroutine to " +
          "yield values.")
      case q"coroutines.this.`package`.call($co.apply(..$args))" =>
        // no need to check further, the call macro will validate the coroutine type
      case q"$co.apply(..$args)" if isCoroutineBlueprint(co.tpe) =>
        c.abort(
          tree.pos,
          "Coroutine blueprints can only be invoked directly inside the coroutine. " +
          "Nested classes and functions should either use the call statement or " +
          "declare another coroutine.")
      case _ =>
        super.traverse(tree)
    }
  }

  def disallowCoroutinesIn(tree: Tree): Unit = {
    for (t <- tree) t match {
      case CoroutineOp(t) => c.abort(t.pos, "Coroutine operations disallowed here.")
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
        var $localvarname = null.asInstanceOf[$tree.tpe]
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
      val nwhile = if (xdecls != Nil) q"""
        ..$xdecls
        var $localvarname = $xident
        while ($localvarname) {
          ${transform(body)}
          ..$xdecls
          localvarname = $xident
        }
      """ else q"""
        while ($cond) {
          ${transform(body)}
        }
      """
      (List(nwhile), q"()")
    case q"do $body while ($cond)" =>
      val (xdecls, xident) = tearExpression(cond)
      val localvarname = TermName(c.freshName())
      val nwhile = if (xdecls != Nil) q"""
        ..$xdecls
        var $localvarname = $xident
        do {
          ${transform(body)}
          ..$xdecls
          localvarname = $xident
        } while ($localvarname)
      """ else q"""
        do {
          ${transform(body)}
        } while ($cond)
      """
      (List(nwhile), q"()")
    case Block(stats, expr) =>
      // block
      val localvarname = TermName(c.freshName())
      val toastats = stats.map(transform)
      val toaexpr = transform(q"$localvarname = $expr")
      val decls = List(
        q"""
          var $localvarname = null.asInstanceOf[${expr.tpe}]
        """,
        q"""
          {
            ..$toastats
            $toaexpr
          }
        """
      )
      (decls, q"$localvarname")
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
      Block(stats.map(transform), transform(expr))
    case t =>
      val (decls, _) = tearExpression(t)
      q"..$decls"
  }

  def transformToTwoOperandForm(args: List[Tree], body: Tree)(
    implicit table: Table
  ): Tree = {
    // TODO save arguments to table

    // recursive transform of the body code
    transform(body)
  }
}
