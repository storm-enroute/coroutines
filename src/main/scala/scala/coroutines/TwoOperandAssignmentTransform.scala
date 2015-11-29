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

  private def validateNestedContext(tree: Tree): Unit = tree match {
    case q"coroutines.this.`package`.coroutine[$_]($_)" =>
      // no need to check further, this is checked in a different expansion
    case q"coroutines.this.`package`.yieldval[$_]($_)" =>
      c.abort(
        tree.pos,
        "The yieldval statement can only be called directly inside the coroutine. " +
        "Nested classes and functions must declare another coroutine to yield values.")
    case q"coroutines.this.`package`.yieldto[$_]($_)" =>
      c.abort(
        tree.pos,
        "The yieldto statement can only be called directly inside the coroutine. " +
        "Nested classes and functions must declare another coroutine to yield values.")
    case q"coroutines.this.`package`.call($co.apply(..$args))" =>
      // no need to check further, the call macro will validate the coroutine type
    case q"$co.apply(..$args)" if isCoroutineBlueprint(co.tpe) =>
      c.abort(
        tree.pos,
        "Coroutine blueprints can only be invoked directly inside the coroutine. " +
        "Nested classes and functions should either use the call statement or " +
        "declare another coroutine.")
    case _ =>
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
      println(argdecls)
      val localvarname = TermName(c.freshName())
      val localvartree = q"val $localvarname = $rident.$method(..$argidents)"
      (rdecls ++ argdecls.flatten ++ List(localvartree), q"$localvarname")
    case q"$r[..$tpts]" if tpts.length > 0 =>
      // type application
      for (tpt <- tpts; t <- tpt) t match {
        case CoroutineOp(t) => c.abort(t.pos, "Types cannot contain coroutine ops.")
        case _ => // fine
      }
      val (rdecls, rident) = tearExpression(r)
      (rdecls, q"$rident[..$tpts]")
    case q"$x(..$args) = $v" =>
      // assignment
      // update
      val (xdecls, xident) = tearExpression(x)
      val (argdecls, argidents) = args.map(tearExpression).unzip
      val (vdecls, vident) = tearExpression(v)
      (xdecls ++ argdecls.flatten ++ vdecls, q"$xident(..$argidents) = $vident")
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
    case q"coroutines.this.`package`.coroutine[$_]($_)" =>
      // no need to check further, this is checked in a different expansion
      tree
    case q"(..$params) => $body" =>
      validateNestedContext(body)
      tree
    // TODO: handle partial functions
    // case q"{ case ..$cs }" =>
    //   validateNestedContext(???)
    //   println(cs)
    //   tree
    case q"if ($cond) $thenbranch else $elsebranch" =>
      val (decls, ncond) = tearExpression(cond)
      q"""
        ..$decls
        if ($ncond) ${transform(thenbranch)} else ${transform(elsebranch)}
      """
    case Block(stats, expr) =>
      Block(stats.map(transform), transform(expr))
    case t =>
      t
  }

  def transformToTwoOperandForm(args: List[Tree], body: Tree)(
    implicit table: Table
  ): Tree = {
    // TODO save arguments to table

    // recursive transform of the body code
    transform(body)
  }
}
