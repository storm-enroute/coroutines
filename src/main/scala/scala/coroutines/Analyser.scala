package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Analyzes coroutine trees and produces control flow graphs.
 */
trait Analyser[C <: Context] {
  val c: Context

  import c.universe._

  case class Zipper(above: Zipper, left: List[Tree], ctor: List[Tree] => Tree) {
    def append(x: Tree) = Zipper(above, x :: left, ctor)
    def isRoot = above == null
    def root = {
      var z = this
      while (z.above != null) z = z.ascend
      z
    }
    def result: Tree = {
      assert(above == null)
      ctor(left.reverse)
    }
    def ascend: Zipper = if (above == null) null else {
      Zipper(above.above, ctor(left.reverse) :: above.left, above.ctor)
    }
    def descend(ctor: List[Tree] => Tree) = Zipper(this, Nil, ctor)
  }

  class VarInfo(
    val uid: Int,
    val origtree: Tree,
    val tpe: Type,
    val sym: Symbol,
    val name: TermName,
    val isArg: Boolean
  ) {
    var stackpos = uid
    def isRefType = tpe <:< typeOf[AnyRef]
    def isValType = tpe <:< typeOf[AnyVal]
    val defaultValue: Tree = {
      if (isRefType) q"null"
      else if (tpe =:= typeOf[Boolean]) q"false"
      else if (tpe =:= typeOf[Byte]) q"0.toByte"
      else if (tpe =:= typeOf[Short]) q"0.toShort"
      else if (tpe =:= typeOf[Char]) q"0.toChar"
      else if (tpe =:= typeOf[Int]) q"0"
      else if (tpe =:= typeOf[Float]) q"0.0f"
      else if (tpe =:= typeOf[Long]) q"0L"
      else if (tpe =:= typeOf[Double]) q"0.0"
      else sys.error(s"Unknown type: $tpe")
    }
    private def encodeLong(t: Tree): Tree = {
      if (tpe =:= typeOf[Int]) q"$t.toLong"
      else sys.error(s"Cannot encode type $tpe as Long.")
    }
    private def decodeLong(t: Tree): Tree = {
      if (tpe =:= typeOf[Int]) q"($t & 0xffffffff).toInt"
      else sys.error(s"Cannot decode type $tpe from Long.")
    }
    val initialValue: Tree = {
      val t = if (isArg) q"$name" else defaultValue
      if (isRefType) t
      else encodeLong(t)
    }
    val stackname = {
      if (isRefType) TermName("refstack")
      else TermName("valstack")
    }
    val stacktpe = {
      if (isRefType) typeOf[AnyRef]
      else typeOf[Long]
    }
    def initialSize: Tree = q"4"
    def pushTree: Tree = q"""
      scala.coroutines.common.Stack.push[$stacktpe](
        c.$stackname, $initialValue, $initialSize)
    """
    def popTree = q"""
      scala.coroutines.common.Stack.pop[$stacktpe](c.$stackname)
    """
  }

  class Table(val lambda: Tree) {
    var varcount = 0
    val all = mutable.LinkedHashMap[Symbol, VarInfo]()
    val topChain = new Chain(this, lambda, null)
    val untyper = new ByTreeUntyper[c.type](c)(lambda)
    def foreach[U](f: ((Symbol, VarInfo)) => U): Unit = all.foreach(f)
    def contains(s: Symbol) = all.contains(s)
    def apply(s: Symbol) = all(s)
    def refvars = all.filter(_._2.isRefType)
    def valvars = all.filter(_._2.isValType)
  }

  class Chain(val table: Table, val origtree: Tree, val parent: Chain) {
    val vars = mutable.LinkedHashMap[Symbol, VarInfo]()
    def newChain(subtree: Tree) = new Chain(table, subtree, this)
    def addVar(valdef: Tree, name: TermName, isArg: Boolean) {
      val sym = valdef.symbol
      val info = new VarInfo(table.varcount, valdef, sym.info, sym, name, isArg)
      vars(sym) = info
      table.all(sym) = info
      table.varcount += 1
    }
    override def toString = {
      val s = s"[${vars.map(_._2.sym).mkString(", ")}] -> "
      if (parent != null) s + parent.toString else s
    }
  }

  class CtrlNode(
    val tree: Tree,
    val ctrlflowtree: Option[Tree],
    val chain: Chain
  ) {
    var successors: List[CtrlNode] = Nil

    def singleSuccessor: Option[CtrlNode] = {
      if (successors.size == 1) Some(successors.head)
      else None
    }

    def forwardSuccessor: CtrlNode = ctrlflowtree match {
      case Some(q"if ($_) $_ else $_") =>
        successors.head
      case None =>
        sys.error(s"Cannot compute forward node for <$tree>.")
    }

    def backwardSuccessor: CtrlNode = ctrlflowtree match {
      case Some(q"if ($_) $_ else $_") =>
        new CtrlNode(q"()", None, chain)
      case None =>
        sys.error(s"Cannot compute backward node for <$tree>.")
    }

    def prettyPrint = {
      val text = new StringBuilder
      var count = 0
      val seen = mutable.Map[CtrlNode, Int]()
      def emit(n: CtrlNode, prefix: String) {
        def shorten(s: String) = {
          if (s.contains('\n')) s.takeWhile(_ != '\n') + "..." else s
        }
        seen(n) = count
        val treerepr = shorten(n.tree.toString)
        text.append(s"$prefix|-> $count: Node($treerepr)\n")
        count += 1
        def emitChild(c: CtrlNode, newPrefix: String) {
          if (seen.contains(c)) {
            text.append(s"$newPrefix|-> label ${seen(c)}")
          } else {
            emit(c, newPrefix)
          }
        }
        if (n.successors.nonEmpty) {
          for (s <- n.successors.tail) {
            emitChild(s, prefix + "|   ")
          }
          emitChild(n.successors.head, prefix)
        }
      }
      emit(this, "")
      text.toString
    }
  }

  object CtrlNode {
    def copyNoSuccessors(n: CtrlNode) =
      new CtrlNode(n.tree, n.ctrlflowtree, n.chain)
  }
}
