package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Analyzes coroutine trees and produces control flow graphs.
 */
trait Analyzer[C <: Context] {
  val c: C

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
    val isArg: Boolean,
    val table: Table
  ) {
    def stackpos = {
      val sametpvars =
        if (isRefType) table.vars.filter(_._2.isRefType)
        else table.vars.filter(_._2.isValType)
      sametpvars.size - 1 - sametpvars.toList.indexWhere(_._2.uid == uid)
    }
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
    def encodeLong(t: Tree): Tree = {
      if (tpe =:= typeOf[Boolean]) q"if ($t) 1L else 0L"
      else if (tpe =:= typeOf[Int]) q"$t.toLong"
      else sys.error(s"Cannot encode type $tpe as Long.")
    }
    def decodeLong(t: Tree): Tree = {
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
    def setTree(x: Tree): Tree = q"""
      scala.coroutines.common.Stack.set[$stacktpe](c.$stackname, $stackpos, $x)
    """
    override def toString = s"VarInfo($uid, $sym)"
  }

  class Table(val lambda: Tree) {
    private var varCount = 0
    private var nodeCount = 0L
    private var subgraphCount = 0L
    val vars = mutable.LinkedHashMap[Symbol, VarInfo]()
    val topChain = new Chain(this, lambda, null)
    val untyper = new ByTreeUntyper[c.type](c)(lambda)
    object names {
      val coroutineParam = TermName(c.freshName())
    }
    def newVarUid(): Int = {
      val c = varCount
      varCount += 1
      c
    }
    def newNodeUid(): Long = {
      val c = nodeCount
      nodeCount += 1
      c
    }
    def newSubgraphUid(): Long = {
      val c = subgraphCount
      subgraphCount += 1
      c
    }
    def foreach[U](f: ((Symbol, VarInfo)) => U): Unit = vars.foreach(f)
    def contains(s: Symbol) = vars.contains(s)
    def apply(s: Symbol) = vars(s)
    def refvars = vars.filter(_._2.isRefType)
    def valvars = vars.filter(_._2.isValType)
  }

  class Chain(val table: Table, val origtree: Tree, val parent: Chain) {
    val vars = mutable.LinkedHashMap[Symbol, VarInfo]()
    def allvars: Iterator[(Symbol, VarInfo)] = {
      vars.iterator ++ (if (parent != null) parent.allvars else Iterator.empty)
    }
    def newChain(subtree: Tree) = new Chain(table, subtree, this)
    def addVar(valdef: Tree, isArg: Boolean) {
      val sym = valdef.symbol
      val name = sym.name.toTermName
      val info = new VarInfo(table.newVarUid, valdef, sym.info, sym, name, isArg, table)
      vars(sym) = info
      table.vars(sym) = info
    }
    override def toString = {
      val s = s"[${vars.map(_._2.sym).mkString(", ")}] -> "
      if (parent != null) s + parent.toString else s
    }
  }

  object ValDecl {
    def unapply(t: Tree): Option[Tree] = t match {
      case q"$_ val $name: $_ = $_" =>
        Some(t)
      case q"$_ var $name: $_ = $_" =>
        Some(t)
      case q"{ $_ val $name: $_ = $_ }" =>
        Some(t.collect({ case t @ q"$_ val $_: $_ = $_" => t }).head)
      case q"{ $_ var $name: $_ = $_ }" =>
        Some(t.collect({ case t @ q"$_ var $_: $_ = $_" => t }).head)
      case _ =>
        None
    }
  }

  def isCoroutineDefType(tpe: Type) = {
    val codefsym = typeOf[Coroutine.Definition[_]].typeConstructor.typeSymbol
    tpe.baseType(codefsym) != NoType
  }

  def coroutineTypeFor(tpe: Type) = {
    val codeftpe = typeOf[Coroutine.Definition[_]].typeConstructor
    appliedType(codeftpe, List(tpe))
  }

  def inferReturnType(body: Tree): Tree = {
    // return type must correspond to the return type of the function literal
    val rettpe = body.tpe

    // return type is the lub of the function return type and yield argument types
    def isCoroutinesPackage(q: Tree) = q match {
      case q"coroutines.this.`package`" => true
      case t => false
    }
    // TODO: ensure that this does not capture constraints from nested class scopes
    // TODO: ensure that this does not collect nested coroutine invocations
    val constraintTpes = body.collect {
      case q"$qual.yieldval[$tpt]($v)" if isCoroutinesPackage(qual) => tpt.tpe
      case q"$qual.yieldto[$tpt]($f)" if isCoroutinesPackage(qual) => tpt.tpe
    }
    tq"${lub(rettpe :: constraintTpes)}"
  }
}
