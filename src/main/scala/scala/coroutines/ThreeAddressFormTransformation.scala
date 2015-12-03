package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Transforms the coroutine body into three address form with restricted control flow
 *  that contains only try-catch statements, while loops, do-while loops, if-statements,
 *  value and variable declarations, pattern matches, nested blocks and function calls.
 *
 *  Newly synthesized variables get mangled fresh names, and existing variable names are
 *  preserved.
 *
 *  Coroutine operations usages are checked for correctness, and nested contexts, such
 *  as function and class declarations, are checked, but not transformed.
 */
trait ThreeAddressFormTransformation[C <: Context] {
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

  private def threeAddressForm(tree: Tree)(
    implicit typer: ByTreeTyper[c.type]
  ): (List[Tree], Tree) = tree match {
    case q"$r.`package`" =>
      // package selection
      (Nil, tree)
    case q"$r.$member" =>
      // selection
      val (rdecls, rident) = threeAddressForm(r)
      val localvarname = TermName(c.freshName("x"))
      val localvartree = q"val $localvarname = $rident.$member"
      (rdecls ++ List(localvartree), q"$localvarname")
    case q"$r.$method[..$tpts](...$paramss)" if tpts.length > 0 || paramss.length > 0 =>
      // application
      // TODO: translate boolean && and || to if statements, then regenerate, to adher
      // to the short-circuit evaluation rules
      for (tpt <- tpts) disallowCoroutinesIn(tpt)
      val (rdecls, rident) = threeAddressForm(r)
      val (pdeclss, pidents) = paramss.map(_.map(threeAddressForm).unzip).unzip
      val localvarname = TermName(c.freshName("x"))
      val localvartree = q"val $localvarname = $rident.$method[..$tpts](...$pidents)"
      (rdecls ++ pdeclss.flatten.flatten ++ List(localvartree), q"$localvarname")
    case q"$r[..$tpts]" if tpts.length > 0 =>
      // type application
      for (tpt <- tpts) disallowCoroutinesIn(tpt)
      val (rdecls, rident) = threeAddressForm(r)
      (rdecls, q"$rident[..$tpts]")
    case q"$x = $v" =>
      // assignment
      val (xdecls, xident) = threeAddressForm(x)
      val (vdecls, vident) = threeAddressForm(v)
      (xdecls ++ vdecls ++ List(q"$xident = $vident"), q"()")
    case q"$x(..$args) = $v" =>
      // update
      val (xdecls, xident) = threeAddressForm(x)
      val (argdecls, argidents) = args.map(threeAddressForm).unzip
      val (vdecls, vident) = threeAddressForm(v)
      (xdecls ++ argdecls.flatten ++ vdecls, q"$xident(..$argidents) = $vident")
    case q"return $_" =>
      // return
      c.abort(tree.pos, "The return statement is not allowed inside coroutines.")
    case q"throw $e" =>
      // throw
      val (edecls, eident) = threeAddressForm(e)
      (edecls, q"throw $eident")
    case q"$x: $tpt" =>
      // ascription
      disallowCoroutinesIn(tpt)
      val (xdecls, xident) = threeAddressForm(x)
      (xdecls, q"xident: $tpt")
    case q"$x: @$annot" =>
      // annotation
      val (xdecls, xident) = threeAddressForm(x)
      (xdecls, q"xident: $annot")
    case q"(..$xs)" if xs.length > 1 =>
      // tuples
      val (xsdecls, xsidents) = xs.map(threeAddressForm).unzip
      (xsdecls.flatten, q"(..$xsidents)")
    case q"if ($cond) $thenbranch else $elsebranch" =>
      // if
      val (conddecls, condident) = threeAddressForm(cond)
      val (thendecls, thenident) = threeAddressForm(thenbranch)
      val (elsedecls, elseident) = threeAddressForm(elsebranch)
      val localvarname = TermName(c.freshName("x"))
      val tpe = typer.typeOf(tree)
      val decls = List(
        q"var $localvarname = null.asInstanceOf[$tpe]",
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
      val localvarname = TermName(c.freshName("x"))
      val ncases = for (cq"$pat => $branch" <- cases) yield {
        disallowCoroutinesIn(pat)
        val (branchdecls, branchident) = threeAddressForm(branch)
        cq"""
          $pat =>
            ..$branchdecls
            $localvarname = $branchident
        """
      }
      val (xdecls, xident) = threeAddressForm(x)
      val tpe = typer.typeOf(tree)
      val decls =
        List(q"var $localvarname = null.asInstanceOf[${tpe.widen}]") ++
        xdecls ++
        List(q"""
          $x match {
            case ..$ncases
          }
        """)
      (decls, q"$localvarname")
    case q"(..$params) => $body" =>
      // function
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"{ case ..$cases }" =>
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"while ($cond) $body" =>
      // while
      val (xdecls, xident) = threeAddressForm(cond)
      val localvarname = TermName(c.freshName("x"))
      val decls = if (xdecls != Nil) {
        xdecls ++ List(
          q"var $localvarname = $xident",
          q"""
            while ($localvarname) {
              ${transform(body)}

              ..$xdecls
              $localvarname = $xident
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
      val (xdecls, xident) = threeAddressForm(cond)
      val localvarname = TermName(c.freshName("x"))
      val decls = if (xdecls != Nil) xdecls ++ List(
        q"var $localvarname = $xident",
        q"""
          do {
            ${transform(body)}

            ..$xdecls
            $localvarname = $xident
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
      val localvarname = TermName(c.freshName("x"))
      val (statdecls, statidents) = stats.map(threeAddressForm).unzip
      val (exprdecls, exprident) = threeAddressForm(q"$localvarname = $expr")
      val tpe = typer.typeOf(expr)
      val decls =
        List(q"var $localvarname = null.asInstanceOf[${tpe.widen}]") ++
        statdecls.flatten ++
        exprdecls
      (decls, q"$localvarname")
    case tpt: TypeTree =>
      // type trees
      disallowCoroutinesIn(tpt)
      (Nil, tree)
    case q"$mods val $v: $tpt = $rhs" =>
      // val
      val (rhsdecls, rhsident) = threeAddressForm(rhs)
      val decls = rhsdecls ++ List(q"$mods val $v: $tpt = $rhsident")
      (decls, q"")
    case q"$mods var $v: $tpt = $rhs" =>
      // var
      val (rhsdecls, rhsident) = threeAddressForm(rhs)
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
    case q"$_ class $_[..$_] $_(...$_) extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
      // class
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"$_ trait $_[..$_] extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
      // trait
      NestedContextValidator.traverse(tree)
      (Nil, tree)
    case q"$_ object $_ extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
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
    implicit typer: ByTreeTyper[c.type]
  ): Tree = tree match {
    case Block(stats, expr) =>
      val (statdecls, statidents) = stats.map(threeAddressForm).unzip
      val (exprdecls, exprident) = threeAddressForm(expr)
      q"""
        ..${statdecls.flatten}

        ..$exprdecls

        $exprident
      """
    case t =>
      val (decls, ident) = threeAddressForm(t)
      q"""
        ..$decls

        $ident
      """
  }

  def transformToThreeAddressForm(rawlambda: Tree): Tree = {
    //val untypedrawlambda = c.untypecheck(rawlambda)
    val typer = new ByTreeTyper[c.type](c)(rawlambda)
    val untypedrawlambda = typer.untypedTree

    // separate to arguments and body
    val (args, body) = untypedrawlambda match {
      case q"(..$args) => $body" => (args, body)
      case t => c.abort(t.pos, "The coroutine takes a single function literal.")
    }

    // recursive transform of the body code
    val transformedBody = transform(body)(typer)
    val untypedtaflambda = q"(..$args) => $transformedBody"
    c.typecheck(untypedtaflambda)
  }
}
