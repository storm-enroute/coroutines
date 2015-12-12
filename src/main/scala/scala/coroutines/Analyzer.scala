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
    def result: Tree = {
      var z = this
      while (z.above != null) z = z.ascend
      z.ctor(z.left.reverse)
    }
    def ascend: Zipper = if (above == null) null else {
      Zipper(above.above, ctor(left.reverse) :: above.left, above.ctor)
    }
    def descend(ctor: List[Tree] => Tree) = Zipper(this, Nil, ctor)
  }

  class VarInfo(
    val uid: Int,
    val origtree: Tree,
    val sym: Symbol,
    val isArg: Boolean,
    val table: Table
  ) {
    val tpe = sym.info
    val name = sym.name.toTermName
    def stackpos = {
      val sametpvars =
        if (isRefType) table.vars.filter(_._2.isRefType)
        else table.vars.filter(_._2.isValType)
      sametpvars.size - 1 - sametpvars.toList.indexWhere(_._2.uid == uid)
    }
    def isUnitType = tpe =:= typeOf[Unit]
    def isRefType = {
      tpe <:< typeOf[AnyRef] || tpe =:= typeOf[Unit] || tpe =:= typeOf[Any]
    }
    def isValType = {
      tpe <:< typeOf[AnyVal] && !(tpe =:= typeOf[Unit]) && !(tpe =:= typeOf[Any])
    }
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
      if (tpe =:= typeOf[Boolean]) q"if ($t) 1L else 0L"
      else if (tpe =:= typeOf[Int]) q"$t.toLong"
      else if (tpe =:= typeOf[Long]) q"$t"
      else if (tpe =:= typeOf[Double]) q"java.lang.Double.doubleToRawLongBits($t)"
      else sys.error(s"Cannot encode type $tpe as Long.")
    }
    private def decodeLong(t: Tree): Tree = {
      if (tpe =:= typeOf[Boolean]) q"($t != 0)"
      else if (tpe =:= typeOf[Int]) q"($t & 0xffffffff).toInt"
      else if (tpe =:= typeOf[Long]) q"$t"
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
    def pushTree(implicit t: Table): Tree = q"""
      scala.coroutines.common.Stack.push[$stacktpe](
        c.$stackname, $initialValue, ${t.initialStackSize})
    """
    def popTree = q"""
      scala.coroutines.common.Stack.pop[$stacktpe](c.$stackname)
    """
    def setTree(coroutine: Tree, x: Tree): Tree = {
      val encoded = {
        if (isUnitType) q"$x.asInstanceOf[AnyRef]"
        else if (isRefType) x
        else encodeLong(x)
      }
      q"""
        scala.coroutines.common.Stack.set[$stacktpe](
          $coroutine.$stackname, $stackpos, $encoded)
      """
    }
    def getTree(coroutine: Tree): Tree = {
      if (isUnitType) q"()"
      else {
        val t = q"""
          scala.coroutines.common.Stack.get[$stacktpe]($coroutine.$stackname, $stackpos)
        """
        if (isRefType) q"$t.asInstanceOf[$tpe]"
        else decodeLong(t)
      }
    }
    override def toString = s"VarInfo($uid, $sym)"
  }

  class Table(private val lambda: Tree) {
    private var varCount = 0
    private var nodeCount = 0L
    private var subgraphCount = 0L
    val vars = mutable.LinkedHashMap[Symbol, VarInfo]()
    val topChain = Chain(new Block, Nil, this, null)
    val untyper = new ByTreeUntyper[c.type](c)(lambda)
    def initialStackSize: Int = 4
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

  class Block {
    val decls = mutable.Map[Symbol, VarInfo]()
    val occurrences = mutable.Map[Symbol, VarInfo]()
    val assignments = mutable.Map[Symbol, VarInfo]()
    override def toString = {
      s"decl = ${decls.map(_._1.name).mkString(", ")}, " +
      s"occ = ${occurrences.map(_._1.name).mkString(", ")}, " +
      s"ass = ${assignments.map(_._1.name).mkString(", ")}"
    }
  }

  case class Chain(
    block: Block, decls: List[(Symbol, VarInfo)], table: Table, parent: Chain
  ) {
    def alldecls: List[(Symbol, VarInfo)] = {
      decls ::: (if (parent != null) parent.alldecls else Nil)
    }
    def contains(s: Symbol): Boolean = {
      decls.exists(_._1 == s) || (parent != null && parent.contains(s))
    }
    def ancestors: List[Chain] = {
      if (parent != null) this :: parent.ancestors else this :: Nil
    }
    def chainForDecl(s: Symbol): Option[Chain] = {
      if (decls.exists(_._1 == s)) Some(this)
      else if (parent != null) parent.chainForDecl(s)
      else None
    }
    def isAssigned(s: Symbol): Boolean = {
      block.assignments.contains(s)
    }
    def isAssignedInAncestors(s: Symbol): Boolean = {
      isAssigned(s) || (parent != null && parent.isAssignedInAncestors(s))
    }
    def isDeclared(s: Symbol): Boolean = {
      block.decls.contains(s)
    }
    def isDeclaredInAncestors(s: Symbol): Boolean = {
      isDeclared(s) || (parent != null && parent.isDeclaredInAncestors(s))
    }
    def isOccurring(s: Symbol): Boolean = {
      block.occurrences.contains(s)
    }
    def isOccurringInAncestors(s: Symbol): Boolean = {
      isOccurring(s) || (parent != null && parent.isOccurringInAncestors(s))
    }
    def withDecl(valdef: Tree, isArg: Boolean): Chain = {
      val sym = valdef.symbol
      val info = table.vars.get(sym) match {
        case Some(info) =>
          info
        case None =>
          new VarInfo(table.newVarUid, valdef, sym, isArg, table)
      }
      table.vars(sym) = info
      Chain(block, (sym, info) :: decls, table, parent)
    }
    def descend = Chain(new Block, Nil, table, this)
    def copyWithoutBlocks: Chain = {
      val nparent = if (parent == null) null else parent.copyWithoutBlocks
      Chain(new Block, decls, table, nparent)
    }
    override def equals(that: Any) = that match {
      case that: AnyRef => this eq that
      case _ => false
    }
    override def hashCode = System.identityHashCode(this)
    override def toString = {
      val s = s"[${decls.map(_._1.name).mkString(", ")}] -> "
      if (parent != null) s + parent.toString else s
    }
    def verboseString: String = {
      val b = block.toString
      val s = s"[${decls.map(_._1.name).mkString(", ")} | <$b>] -> "
      if (parent != null) s + parent.verboseString else s
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

  def isCoroutineBlueprint(tpe: Type) = {
    val codefsym = typeOf[Coroutine.Blueprint[_]].typeConstructor.typeSymbol
    tpe.baseType(codefsym) != NoType
  }

  def coroutineTypeFor(tpe: Type) = {
    val codeftpe = typeOf[Coroutine.Blueprint[_]].typeConstructor
    appliedType(codeftpe, List(tpe))
  }

  object CoroutineOp {
    def unapply(t: Tree): Option[Tree] = t match {
      case q"coroutines.this.`package`.coroutine[$_]($_)" =>
        Some(t)
      case q"coroutines.this.`package`.yieldval[$_]($_)" =>
        Some(t)
      case q"coroutines.this.`package`.yieldto[$_]($_)" =>
        Some(t)
      case q"coroutines.this.`package`.call($co.apply(..$args))" =>
        Some(t)
      case q"$co.apply(..$args)" if isCoroutineBlueprint(co.tpe) =>
        Some(t)
      case _ =>
        None
    }
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
    tq"${lub(rettpe :: constraintTpes).widen}"
  }
}
