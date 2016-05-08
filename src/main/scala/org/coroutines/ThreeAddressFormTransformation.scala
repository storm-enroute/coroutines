package org.coroutines



import org.coroutines.common._
import scala.collection._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Transforms the coroutine body into three address form with restricted control flow
 *  that contains only try-catch statements, while loops, if-statements, value and
 *  variable declarations, pattern matches, nested blocks and function calls.
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

  class NestedContextValidator(implicit typer: ByTreeTyper[c.type])
  extends Traverser {
    override def traverse(tree: Tree): Unit = tree match {
      case q"$qual.coroutine[$_]($_)" if isCoroutinesPkg(qual) =>
        // no need to check further, this is checked in a different expansion
      case q"$qual.yieldval[$_]($_)" if isCoroutinesPkg(qual) =>
        c.abort(
          tree.pos,
          "The yieldval statement only be invoked directly inside the coroutine. " +
          "Nested classes, functions or for-comprehensions, should either use the " +
          "call statement or declare another coroutine.")
      case q"$qual.yieldto[$_]($_)" if isCoroutinesPkg(qual) =>
        c.abort(
          tree.pos,
          "The yieldto statement only be invoked directly inside the coroutine. " +
          "Nested classes, functions or for-comprehensions, should either use the " +
          "call statement or declare another coroutine.")
      case q"$qual.call($co.apply(..$args))" if isCoroutinesPkg(qual) =>
        // no need to check further, the call macro will validate the coroutine type
      case q"$co.apply(..$args)" if isCoroutineDefMarker(typer.typeOf(co)) =>
        c.abort(
          tree.pos,
          "Coroutine blueprints can only be invoked directly inside the coroutine. " +
          "Nested classes, functions or for-comprehensions, should either use the " +
          "call statement or declare another coroutine.")
      case q"$co.apply[..$_](..$args)(..$_)"
        if isCoroutineDefMarker(typer.typeOf(co)) =>
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
    case q"$r.$member" if !tree.symbol.isPackage =>
      // selection
      val (rdecls, rident) = threeAddressForm(r)
      val localvarname = TermName(c.freshName("x"))
      val localvartree = q"val $localvarname = $rident.$member"
      (rdecls ++ List(localvartree), q"$localvarname")
    case q"$r.&&($arg)"
      if typer.typeOf(r) =:= typeOf[Boolean] && typer.typeOf(arg) =:= typeOf[Boolean] =>
      // short-circuit boolean and
      val (conddecls, condident) = threeAddressForm(r)
      val (thendecls, thenident) = threeAddressForm(arg)
      val localvarname = TermName(c.freshName("x"))
      val decls = List(
        q"var $localvarname = null.asInstanceOf[Boolean]",
        q""" 
          ..$conddecls
          if ($condident) {
            ..$thendecls
            $localvarname = $thenident
          } else {
            $localvarname = false
          }
        """
      )
      (decls, q"$localvarname")
    case q"$r.||($arg)"
      if typer.typeOf(r) =:= typeOf[Boolean] && typer.typeOf(arg) =:= typeOf[Boolean] =>
      // short-circuit boolean or
      val (conddecls, condident) = threeAddressForm(r)
      val (elsedecls, elseident) = threeAddressForm(arg)
      val localvarname = TermName(c.freshName("x"))
      val decls = List(
        q"var $localvarname = null.asInstanceOf[Boolean]",
        q""" 
          ..$conddecls
          if ($condident) {
            $localvarname = true
          } else {
            ..$elsedecls
            $localvarname = $elseident
          }
        """
      )
      (decls, q"$localvarname")
    case q"$selector[..$tpts](...$paramss)" if tpts.length > 0 || paramss.length > 0 =>
      // application
      val (rdecls, newselector) = selector match {
        case q"$r.$method" =>
          val (rdecls, rident) = threeAddressForm(r)
          (rdecls, q"$rident.$method")
        case q"${method: TermName}" =>
          (Nil, q"$method")
      }
      for (tpt <- tpts) disallowCoroutinesIn(tpt)
      val (pdeclss, pidents) = paramss.map(_.map(threeAddressForm).unzip).unzip
      val localvarname = TermName(c.freshName("x"))
      val localvartree = q"val $localvarname = $newselector[..$tpts](...$pidents)"
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
    case q"$x: $tpt" =>
      // ascription
      disallowCoroutinesIn(tpt)
      val (xdecls, xident) = threeAddressForm(x)
      (xdecls, q"$xident: $tpt")
    case q"$x: @$annot" =>
      // annotation
      val (xdecls, xident) = threeAddressForm(x)
      (xdecls, q"$xident: $annot")
    case q"(..$xs)" if xs.length > 1 =>
      // tuples
      val (xsdecls, xsidents) = xs.map(threeAddressForm).unzip
      (xsdecls.flatten, q"(..$xsidents)")
    case q"throw $expr" =>
      // throw
      val (decls, ident) = threeAddressForm(expr)
      val ndecls = decls ++ List(q"throw $ident")
      (ndecls, q"()")
    case q"try $body catch { case ..$cases } finally $expr" =>
      // try
      val tpe = typer.typeOf(tree)
      val localvarname = TermName(c.freshName("x"))
      val exceptionvarname = TermName(c.freshName("e"))
      val bindingname = TermName(c.freshName("t"))
      val (bodydecls, bodyident) = threeAddressForm(body)
      val (exprdecls, exprident) = threeAddressForm(expr)
      val matchcases =
        cases :+ cq"${pq"null"} =>" :+ cq"${pq"_"} => throw $exceptionvarname"
      val exceptionident = q"$exceptionvarname"
      val matchbody = q"$exceptionident match { case ..$matchcases }"
      typer.typeOf(matchbody) = typer.typeOf(tree)
      typer.typeOf(exceptionident) = typeOf[Throwable]
      val (matchdecls, matchident) = threeAddressForm(matchbody)
      val ndecls = List(
        q"var $localvarname = null.asInstanceOf[$tpe]",
        q"var $exceptionvarname: Throwable = null",
        q"""
          try {
            ..$bodydecls

            $localvarname = $bodyident
          } catch {
            case $bindingname: Throwable => $exceptionvarname = $bindingname
          }
        """
      ) ++ List(if (expr == q"") q"""
          ..$matchdecls

          $localvarname = $matchident
        """ else q"""
          try {
            ..$matchdecls

            $localvarname = $matchident
          } finally {
            $expr
          }
        """
      )
      (ndecls, q"$localvarname")
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
    case q"$expr match { case ..$cases }" =>
      // pattern match
      val localvarname = TermName(c.freshName("x"))
      val (exdecls, exident) = threeAddressForm(expr)
      val tpe = typer.typeOf(tree)
      val extpe = typer.typeOf(expr)
      val ncases = for (cq"$pat => $branch" <- cases) yield {
        disallowCoroutinesIn(pat)
        val (branchdecls, branchident) = threeAddressForm(branch)
        val isWildcard = pat match {
          case pq"_" => true
          case _ => false
        }
        val checkcases =
          if (isWildcard) List(cq"$pat => true")
          else List(cq"$pat => true", cq"_ => false")
        val patdecl = q"val $pat: $exident @scala.unchecked = $exident"
        val body = q"""
          ..$patdecl

          ..$branchdecls

          $localvarname = $branchident
        """
        (q"$exident match { case ..$checkcases }", body)
      }
      val patternmatch =
        ncases.foldRight(q"throw new scala.MatchError($exident)": Tree) {
          case ((patternmatch, ifbranch), elsebranch) =>
            q"if ($patternmatch) $ifbranch else $elsebranch"
        }
      val decls =
        List(q"var $localvarname = null.asInstanceOf[${tpe.widen}]") ++
        exdecls ++
        List(patternmatch)
      (decls, q"$localvarname")
    case q"(..$params) => $body" =>
      // function
      new NestedContextValidator().traverse(tree)
      (Nil, tree)
    case q"{ case ..$cases }" =>
      // partial function
      new NestedContextValidator().traverse(tree)
      (Nil, tree)
    case q"while ($cond) $body" =>
      // while
      val (xdecls0, xident0) = threeAddressForm(cond)
      // TODO: This is a temporary fix. It is very dangerous, since it makes the
      // transformation take O(2^n) time in the depth of the tree.
      //
      // The correct solution is to duplicate the trees so that duplicate value decls in
      // the two trees get fresh names.
      val (xdecls1, xident1) = threeAddressForm(cond)
      val localvarname = TermName(c.freshName("x"))
      val decls = if (xdecls0 != Nil) {
        xdecls0 ++ List(
          q"var $localvarname = $xident0",
          q"""
            while ($localvarname) {
              ${transform(body)}

              ..$xdecls1
              $localvarname = $xident1
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
      // TODO: This translation is a temporary fix, and can result in O(2^n) time. The
      // correct solution is to transform the subtree once, duplicate the transformed
      // trees and rename the variables.
      val (xdecls0, xident0) = threeAddressForm(cond)
      val (xdecls1, xident1) = threeAddressForm(cond)
      val localvarname = TermName(c.freshName("x"))
      val decls = if (xdecls0 != Nil) List(
        q"""
          {
            ${transform(body)}
          }
        """
      ) ++ xdecls0 ++ List(
        q"var $localvarname = $xident0",
        q"""
          while ($localvarname) {
            ${transform(body)}

            ..$xdecls1

            $localvarname = $xident1
          }
        """
      ) else List(
        q"""
          {
            ${transform(body)}
          }

          while ($cond) {
            ${transform(body)}
          }
        """
      )
      (decls, q"()")
    case q"for (..$enums) $body" =>
      // for loop
      for (e <- enums) new NestedContextValidator().traverse(e)
      new NestedContextValidator().traverse(body)
      (Nil, tree)
    case q"for (..$enums) yield $body" =>
      // for-yield loop
      for (e <- enums) new NestedContextValidator().traverse(e)
      new NestedContextValidator().traverse(body)
      (Nil, tree)
    case q"new { ..$edefs } with ..$bases { $self => ..$stats }" =>
      // new
      if (!isCoroutineDef(typer.typeOf(tree))) {
        // if this class was not generated from a coroutine declaration, then validate
        // the nested context
        new NestedContextValidator().traverse(tree)
      }
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
      new NestedContextValidator().traverse(tree)
      (Nil, tree)
    case q"$mods type $tpname[..$tparams] = $tpt" =>
      // type
      new NestedContextValidator().traverse(tree)
      (Nil, tree)
    case q"$_ class $_[..$_] $_(...$_) extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
      // class
      new NestedContextValidator().traverse(tree)
      (Nil, tree)
    case q"$_ trait $_[..$_] extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
      // trait
      new NestedContextValidator().traverse(tree)
      (Nil, tree)
    case q"$_ object $_ extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
      // object
      new NestedContextValidator().traverse(tree)
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
    // println(untypedtaflambda)
    c.typecheck(untypedtaflambda)
  }
}
