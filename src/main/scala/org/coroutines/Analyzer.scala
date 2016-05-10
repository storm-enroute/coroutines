package org.coroutines



import org.coroutines.common._
import scala.collection._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Declares basic data types and analysis utilities.
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
    def ascend: Zipper = if (above == null) sys.error("cannot ascend") else {
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
    private var rawstackpos: (Int, Int) = null
    val tpe = sym.info
    val name = sym.name.toTermName
    def stackpos: (Int, Int) = {
      assert(rawstackpos != null, s"Variable '$sym' without computed stack position.")
      rawstackpos
    }
    def isWide = tpe =:= typeOf[Double] || tpe =:= typeOf[Long]
    def width: Int = if (isWide) 2 else 1
    def stackpos_=(v: (Int, Int)) = rawstackpos = v
    def isUnitType = tpe =:= typeOf[Unit]
    def isAnyType = tpe =:= typeOf[Any]
    def isRefType = Analyzer.this.isRefType(tpe)
    def isValType = Analyzer.this.isValType(tpe)
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
    private def encodeInt(t: Tree): Tree = {
      if (tpe =:= typeOf[Boolean]) q"if ($t) 1 else 0"
      else if (tpe =:= typeOf[Byte]) q"$t.toInt"
      else if (tpe =:= typeOf[Short]) q"$t.toInt"
      else if (tpe =:= typeOf[Char]) q"$t.toInt"
      else if (tpe =:= typeOf[Int]) q"$t"
      else if (tpe =:= typeOf[Float]) q"_root_.java.lang.Float.floatToIntBits($t)"
      else sys.error(s"Cannot encode type $tpe as Int.")
    }
    private def encodeWide(t: Tree): (Tree, Tree) = {
      val nme = TermName(c.freshName("v"))
      val enc =
        if (tpe =:= typeOf[Long]) q"$t"
        else if (tpe =:= typeOf[Double]) q"_root_.java.lang.Double.doubleToLongBits($t)"
        else sys.error(s"Cannot encode wide type $tpe.")
      (q"val $nme = $enc", q"$nme")
    }
    private def decodeInt(t: Tree): Tree = {
      if (tpe =:= typeOf[Boolean]) q"($t != 0)"
      else if (tpe =:= typeOf[Byte]) q"$t.toByte"
      else if (tpe =:= typeOf[Short]) q"$t.toShort"
      else if (tpe =:= typeOf[Char]) q"$t.toChar"
      else if (tpe =:= typeOf[Int]) q"$t"
      else if (tpe =:= typeOf[Float]) q"_root_.java.lang.Float.intBitsToFloat($t)"
      else sys.error(s"Cannot decode type $tpe from Long.")
    }
    private def decodeWide(t: Tree): Tree = {
      if (tpe =:= typeOf[Long]) q"$t"
      else if (tpe =:= typeOf[Double]) q"_root_.java.lang.Double.longBitsToDouble($t)"
      else sys.error(s"Cannot decode wide type $tpe.")
    }
    val initialValue: Tree = {
      val t = if (isArg) q"$name" else defaultValue
      if (isRefType) t else t
    }
    val stackname = {
      if (isRefType) TermName("$refstack")
      else TermName("$valstack")
    }
    val stacktpe = {
      if (isRefType) typeOf[AnyRef] else typeOf[Int]
    }
    def pushTree(implicit t: Table): Tree = {
      if (isWide) {
        val (decl, ident) = encodeWide(initialValue)
        q"""
          $decl

          _root_.org.coroutines.common.Stack.push[$stacktpe](
            $$c.$stackname, ($ident & 0xffffffff).toInt, ${t.initialStackSize})
          _root_.org.coroutines.common.Stack.push[$stacktpe](
            $$c.$stackname, (($ident >>> 32) & 0xffffffff).toInt, ${t.initialStackSize})
        """
      } else q"""
        _root_.org.coroutines.common.Stack.push[$stacktpe](
          $$c.$stackname, ${encodeInt(initialValue)}, ${t.initialStackSize})
      """
    }
    def popTree = {
      if (isWide) q"""
        _root_.org.coroutines.common.Stack.pop[$stacktpe]($$c.$stackname)
        _root_.org.coroutines.common.Stack.pop[$stacktpe]($$c.$stackname)
      """ else q"""
        _root_.org.coroutines.common.Stack.pop[$stacktpe]($$c.$stackname)
      """
    }
    def storeTree(coroutine: Tree, x: Tree): Tree = {
      if (isWide) {
        val (decl, v) = encodeWide(x)
        q"""
          $decl

          _root_.org.coroutines.common.Stack.set[$stacktpe](
            $coroutine.$stackname, ${stackpos._1 + 0}, ($v & 0xffffffff).toInt)
          _root_.org.coroutines.common.Stack.set[$stacktpe](
            $coroutine.$stackname, ${stackpos._1 + 1}, (($v >>> 32) & 0xffffffff).toInt)
        """
      } else {
        val encoded = {
          if (isUnitType) q"$x.asInstanceOf[AnyRef]"
          else if (isAnyType) q"$x.asInstanceOf[AnyRef]"
          else if (isRefType) x
          else encodeInt(x)
        }
        q"""
          _root_.org.coroutines.common.Stack.set[$stacktpe](
            $coroutine.$stackname, ${stackpos._1}, $encoded)
        """
      }
    }
    def loadTree(coroutine: Tree): Tree = {
      if (isWide) {
        val nme0 = TermName(c.freshName("v"))
        val nme1 = TermName(c.freshName("v"))
        val decoded = decodeWide(q"($nme1.toLong << 32) | $nme0")
        q"""
          val $nme0 = _root_.org.coroutines.common.Stack.get[$stacktpe](
            $coroutine.$stackname, ${stackpos._1 + 0})
          val $nme1 = _root_.org.coroutines.common.Stack.get[$stacktpe](
            $coroutine.$stackname, ${stackpos._1 + 1})
          $decoded
        """
      } else {
        if (isUnitType) q"()"
        else {
          val t = q"""
            _root_.org.coroutines.common.Stack.get[$stacktpe](
              $coroutine.$stackname, ${stackpos._1})
          """
          if (isRefType) q"$t.asInstanceOf[$tpe]"
          else decodeInt(t)
        }
      }
    }
    override def toString = s"VarInfo($uid, $sym)"
  }

  class Table(private val lambda: Tree) {
    val q"(..$args) => $body" = lambda
    val yieldType = inferYieldType(body)
    val returnType = inferReturnType(body)
    val returnValueMethodName = Analyzer.this.returnValueMethodName(returnType.tpe)
    private var varCount = 0
    private var nodeCount = 0L
    private var subgraphCount = 0L
    val vars = mutable.LinkedHashMap[Symbol, VarInfo]()
    val topChain = Chain(new BlockInfo(None), Nil, this, null)
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

  class BlockInfo(val tryuids: Option[(Long, Long)]) {
    val decls = mutable.LinkedHashMap[Symbol, VarInfo]()
    val occurrences = mutable.LinkedHashMap[Symbol, VarInfo]()
    val assignments = mutable.LinkedHashMap[Symbol, VarInfo]()
    def copyWithoutVars = new BlockInfo(tryuids)
    override def toString = {
      s"[decl = ${decls.map(_._1.name).mkString(", ")}, " +
      s"occ = ${occurrences.map(_._1.name).mkString(", ")}, " +
      s"ass = ${assignments.map(_._1.name).mkString(", ")}, " +
      s"tryuids = $tryuids]"
    }
  }

  case class Chain(
    info: BlockInfo, decls: List[(Symbol, VarInfo)], table: Table, parent: Chain
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
    def isDescendantOf(that: Chain): Boolean = {
      (this.info == that.info && this.decls.length >= that.decls.length) ||
        (parent != null && parent.isDescendantOf(that))
    }
    def isAssigned(s: Symbol): Boolean = {
      info.assignments.contains(s)
    }
    def isAssignedInAncestors(s: Symbol): Boolean = {
      isAssigned(s) || (parent != null && parent.isAssignedInAncestors(s))
    }
    def isDeclared(s: Symbol): Boolean = {
      info.decls.contains(s)
    }
    def isDeclaredInAncestors(s: Symbol): Boolean = {
      isDeclared(s) || (parent != null && parent.isDeclaredInAncestors(s))
    }
    def isOccurring(s: Symbol): Boolean = {
      info.occurrences.contains(s)
    }
    def isOccurringInAncestors(s: Symbol): Boolean = {
      isOccurring(s) || (parent != null && parent.isOccurringInAncestors(s))
    }
    def withDecl(valdef: Tree, isArg: Boolean): Chain = {
      val sym = valdef.symbol
      val varinfo = table.vars.get(sym) match {
        case Some(varinfo) =>
          varinfo
        case None =>
          new VarInfo(table.newVarUid, valdef, sym, isArg, table)
      }
      table.vars(sym) = varinfo
      Chain(info, (sym, varinfo) :: decls, table, parent)
    }
    def takeDecls(n: Int) = Chain(info, decls.take(n), table, parent)
    def descend(tryuids: Option[(Long, Long)] = None) =
      Chain(new BlockInfo(tryuids), Nil, table, this)
    def copyWithoutBlocks: Chain = {
      val nparent = if (parent == null) null else parent.copyWithoutBlocks
      Chain(info.copyWithoutVars, decls, table, nparent)
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
      val b = info.toString
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

  def isCoroutineDef(tpe: Type) = {
    val codefsym = typeOf[Coroutine[_, _]].typeConstructor.typeSymbol
    tpe.baseType(codefsym) != NoType
  }

  def isCoroutineDefMarker(tpe: Type) = {
    val codefsym = typeOf[Coroutine.DefMarker[_]].typeConstructor.typeSymbol
    tpe.baseType(codefsym) != NoType
  }

  def isCoroutineDefSugar0(tpe: Type) = {
    val codefsym0 = typeOf[~~~>[_, _]].typeConstructor.typeSymbol
    def hasBase(sym: Symbol) = tpe.baseType(sym) != NoType
    hasBase(codefsym0)
  }

  def isCoroutineDefSugar(tpe: Type) = {
    val codefsym0 = typeOf[~~~>[_, _]].typeConstructor.typeSymbol
    val codefsym1 = typeOf[~~>[_, _]].typeConstructor.typeSymbol
    val codefsym2 = typeOf[~>[_, _]].typeConstructor.typeSymbol
    def hasBase(sym: Symbol) = tpe.baseType(sym) != NoType
    hasBase(codefsym0) || hasBase(codefsym1) || hasBase(codefsym2)
  }

  def coroutineMethodArgs(tpe: Type): List[Type] =
    if (!isCoroutineDefSugar(tpe)) Nil
    else if (isCoroutineDefSugar0(tpe)) Nil
    else {
      val (ytpe, rtpe) = coroutineYieldReturnTypes(tpe)
      val codefsym1 = typeOf[~~>[_, _]].typeConstructor.typeSymbol
      tpe.baseType(codefsym1) match {
        case TypeRef(_, _, List(_, _)) => return List(ytpe, rtpe)
        case _ =>
      }
      val codefsym2 = typeOf[~>[_, _]].typeConstructor.typeSymbol
      val tupletpe = tpe.baseType(codefsym2) match {
        case TypeRef(_, _, List(tpe, _)) => tpe
      }
      val tuple2sym = typeOf[(_, _)].typeConstructor.typeSymbol
      tupletpe.baseType(tuple2sym) match {
        case TypeRef(_, _, tpargs) => return tpargs ++ List(ytpe, rtpe)
        case _ =>
      }
      val tuple3sym = typeOf[(_, _, _)].typeConstructor.typeSymbol
      tupletpe.baseType(tuple3sym) match {
        case TypeRef(_, _, tpargs) => return tpargs ++ List(ytpe, rtpe)
        case _ =>
      }
      sys.error(s"Not a coroutine sugar type with type params: $tpe")
    }

  def coroutineYieldReturnTypes(tpe: Type) = {
    val codefsym = typeOf[Coroutine.DefMarker[_]].typeConstructor.typeSymbol
    val tuplesym = typeOf[(_, _)].typeConstructor.typeSymbol
    tpe.baseType(codefsym) match {
      case TypeRef(_, sym, List(typetuple)) =>
        typetuple.baseType(tuplesym) match {
          case TypeRef(_, sym, List(yldtpe, rettpe)) => (yldtpe, rettpe)
        }
    }
  }

  def coroutineTypeFor(tpe: Type) = {
    val codeftpe = typeOf[Coroutine[_, _]].typeConstructor
    appliedType(codeftpe, List(tpe))
  }

  object CoroutineOp {
    def unapply(t: Tree): Option[Tree] = t match {
      case q"$qual.`package`.coroutine[$_]($_)" if isCoroutinesPkg(qual) =>
        Some(t)
      case q"$qual.`package`.yieldval[$_]($_)" if isCoroutinesPkg(qual) =>
        Some(t)
      case q"$qual.`package`.yieldto[$_]($_)" if isCoroutinesPkg(qual) =>
        Some(t)
      case q"$qual.`package`.call($_.apply(..$_))" if isCoroutinesPkg(qual) =>
        Some(t)
      case q"$co.apply(..$_)" if isCoroutineDefMarker(co.tpe) =>
        Some(t)
      case q"$co.apply[..$_](..$_)(..$_)" if isCoroutineDefSugar(co.tpe) =>
        Some(t)
      case _ =>
        None
    }
  }

  // return type is the lub of the function return type and yield argument types
  def isCoroutinesPkg(q: Tree) = q match {
    case q"org.coroutines.`package`" => true
    case q"coroutines.this.`package`" => true
    case t => false
  }

  def isRefType(tpe: Type) = !isValType(tpe)

  def isValType(tpe: Type) = {
    tpe =:= typeOf[Boolean] ||
    tpe =:= typeOf[Byte] ||
    tpe =:= typeOf[Short] ||
    tpe =:= typeOf[Char] ||
    tpe =:= typeOf[Int] ||
    tpe =:= typeOf[Float] ||
    tpe =:= typeOf[Long] ||
    tpe =:= typeOf[Double]
  }

  def typeChar(tpe: Type): Char = {
    if (isRefType(tpe)) 'L'
    else if (tpe =:= typeOf[Boolean]) 'Z'
    else if (tpe =:= typeOf[Byte]) 'B'
    else if (tpe =:= typeOf[Short]) 'S'
    else if (tpe =:= typeOf[Char]) 'C'
    else if (tpe =:= typeOf[Int]) 'I'
    else if (tpe =:= typeOf[Float]) 'F'
    else if (tpe =:= typeOf[Long]) 'J'
    else if (tpe =:= typeOf[Double]) 'D'
    else sys.error("unreachable")
  }

  def returnValueMethodName(tpe: Type) = TermName("$returnvalue$" + typeChar(tpe))

  def inferYieldType(body: Tree): Tree = {
    // yield type must correspond to the `yieldval`, `yieldto` and coroutine-apply args
    val constraintTpes = body.collect {
      case q"$qual.yieldval[$tpt]($_)" if isCoroutinesPkg(qual) =>
        tpt.tpe
      case q"$co.apply(..$_)" if isCoroutineDefMarker(co.tpe) =>
        coroutineYieldReturnTypes(co.tpe)._1
      case q"$co.apply[..$_](..$_)(..$_)" if isCoroutineDefSugar(co.tpe) =>
        coroutineYieldReturnTypes(co.tpe)._1
    }
    tq"${lub(constraintTpes).widen}"
  }

  def inferReturnType(body: Tree): Tree = tq"${lub(body.tpe :: Nil).widen}"
}
