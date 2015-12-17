package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.coroutines.common.Cache._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Generates control flow graphs, and converts CFG nodes to ASTs.
 */
trait CfgGenerator[C <: Context] {
  self: Analyzer[C] =>

  val c: C

  import c.universe._

  private sealed trait CanCall

  private object Permissions {
    implicit object canEmit extends CanCall
  }

  abstract class Node {
    val successors = mutable.Buffer[Node]()

    val uid: Long

    val tree: Tree

    def chain: Chain

    def updateBlock()(implicit table: Table) {
      for (t <- code) {
        if (table.contains(t.symbol)) {
          chain.block.occurrences(t.symbol) = table(t.symbol)
        }
      }
    }

    def copyWithoutSuccessors(nch: Chain): Node

    def code: Tree = tree

    def value: Tree = tree

    def isEmptyAtReturn = false

    def className: String = getClass.getName.dropWhile(_ != '$')

    final def dfs: Seq[Node] = {
      val seen = mutable.LinkedHashSet[Node]()
      def traverse(n: Node) {
        if (!seen(n)) {
          seen += n
          for (sn <- n.successors) traverse(sn)
        }
      }
      traverse(this)
      seen.toSeq
    }

    final def emitCode(z: Zipper, subgraph: SubCfg)(implicit t: Table): Zipper = {
      val seen = mutable.Set[Node]()
      val finalzipper = this.markEmit(z, seen, subgraph)
      finalzipper
    }

    def emit(
      z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
    )(implicit cc: CanCall, t: Table): Zipper

    final def markEmit(
      z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
    )(implicit t: Table): Zipper = {
      import Permissions.canEmit
      if (!seen(this)) {
        seen += this
        this.emit(z, seen, subgraph)
      } else z
    }

    def extract(
      prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
      subgraph: SubCfg
    )(implicit table: Table): Node

    protected def addSuccessorsToNodeFront(ctx: ExtractSubgraphContext) {
      // add successors to node front
      for (s <- this.successors) if (!ctx.seenEntryPoints(s)) {
        ctx.seenEntryPoints += s
        ctx.nodefront.enqueue(s)
      }
    }

    def stackVars(subgraph: SubCfg): List[Symbol] = Nil

    protected def storePointVarsInChain(subgraph: SubCfg): List[(Symbol, VarInfo)] =
      for {
        (sym, info) <- chain.alldecls
        if subgraph.mustStoreVar(this, sym)
      } yield (sym, info)

    protected def genSaveState(subgraph: SubCfg)(implicit t: Table): List[Tree] = {
      val cparam = t.names.coroutineParam
      // store state for non-val variables in scope
      val stackstores = for ((sym, info) <- storePointVarsInChain(subgraph)) yield {
        info.storeTree(q"${t.names.coroutineParam}", q"${info.name}")
      }
      // update pc state
      val pc = subgraph.exitSubgraphs(this).uid
      val pcstackset = q"""
        scala.coroutines.common.Stack.update($cparam.$$pcstack, $pc.toShort)
      """
      pcstackset :: stackstores.toList
    }

    protected def genExit(n: Node, subgraph: SubCfg)(implicit t: Table): Tree = {
      val cparam = t.names.coroutineParam
      val untypedtree = t.untyper.untypecheck(n.value)
      q"""
        $$pop($cparam)
        if (scala.coroutines.common.Stack.isEmpty($cparam.$$costack)) {
          $$assignresult($cparam, $untypedtree)
        } else {
          import scala.coroutines.Permission.canCall
          $cparam.$$target = $cparam
          scala.coroutines.common.Stack.top($cparam.$$costack)
            .$$returnvalue($cparam, $untypedtree)
        }
        return
      """
    }

    def prettyPrint = {
      val text = new StringBuilder
      var count = 0
      val seen = mutable.Map[Node, Int]()
      def emit(n: Node, prefix: String) {
        def shorten(s: String) = {
          if (s.contains('\n')) s.takeWhile(_ != '\n') + "..." else s
        }
        seen(n) = count
        val treerepr = shorten(n.tree.toString)
        val name = n.className
        val hc = System.identityHashCode(n)
        text.append(
          s"$prefix|-> $count: $name(uid = ${n.uid}, $hc) " +
          s"<$treerepr> ${n.chain.toString.take(50)}\n")
        count += 1
        def emitChild(c: Node, newPrefix: String) {
          if (seen.contains(c)) {
            val sn = seen(c)
            val hc = System.identityHashCode(c)
            text.append(s"$newPrefix|-> label $sn (uid = ${c.uid}, $hc)\n")
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

  object Node {
    case class If(enduid: Long, tree: Tree, chain: Chain, uid: Long) extends Node {
      override def code = {
        val q"if ($cond) $_ else $_" = tree
        cond
      }
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        val q"if ($cond) $_ else $_" = tree
        val newZipper = Zipper(null, Nil, trees => q"..$trees")
        val newSeen = subgraph.all.get(enduid) match {
          case Some(end) => mutable.Set(end)
          case None => mutable.Set[Node]()
        }
        val elsen = this.successors(1)
        val thenn = this.successors(0)
        val elseb = elsen.markEmit(newZipper, newSeen, subgraph).result
        val thenb = thenn.markEmit(newZipper, newSeen, subgraph).result
        val untypedcond = table.untyper.untypecheck(cond)
        val iftree = q"if ($untypedcond) $thenb else $elseb"
        val z1 = z.append(iftree)
        subgraph.all.get(enduid) match {
          case Some(end) => end.markEmit(z1, seen, subgraph)
          case None => z1
        }
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.descend
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = If(enduid, tree, nch, uid)
    }

    case class IfEnd(chain: Chain, uid: Long) extends Node {
      val tree: Tree = q""
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        if (successors.length == 1) {
          if (successors.head.isEmptyAtReturn) {
            val exittree = genExit(this, subgraph)
            z.append(exittree)
          } else {
            successors.head.markEmit(z, seen, subgraph)
          }
        } else if (successors.length == 0) {
          // do nothing
          val exittree = genExit(this, subgraph)
          z.append(exittree)
        } else sys.error(s"Multiple successors for <$tree>.")
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.parent
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = IfEnd(nch, uid)
      override def isEmptyAtReturn = {
        if (successors.length == 0) true
        else successors.head.isEmptyAtReturn
      }
    }

    case class While(tree: Tree, chain: Chain, uid: Long) extends Node {
      override def code = {
        val q"while ($cond) $_" = tree
        cond
      }
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        val q"while ($cond) $body" = tree
        val untypedcond = table.untyper.untypecheck(cond)
        // val z1 = z.descend(trees => q"while ($untypedcond) ..$trees")
        // successors.head.markEmit(z1, seen, subgraph)
        val endnode = successors.last
        val z1 = z.descend(trees => q"while ($untypedcond) ..$trees")
        val newZipper = Zipper(null, Nil, trees => q"..$trees")
        val newSeen = mutable.Set(endnode)
        val newBody = successors.head.markEmit(newZipper, newSeen, subgraph).result
        val z2 = z1.append(newBody)
        endnode.markEmit(z2, seen, subgraph)
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.descend
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        val s = this.successors.head
        if (!seen.contains(s)) {
          s.extract(nthis.chain, seen, ctx, subgraph)
        }
        val end = this.successors.last
        if (!seen.contains(end)) {
          end.extract(nthis.chain, seen, ctx, subgraph)
        }
        nthis.successors ++= this.successors.map(seen)

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = While(tree, nch, uid)
    }

    case class WhileEnd(chain: Chain, uid: Long) extends Node {
      val tree: Tree = q""
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        if (successors.length == 1) {
          // do nothing
          val z1 = z.ascend
          successors.head.markEmit(z1, seen, subgraph)
        } else if (successors.length == 2) {
          val z1 = z.ascend
          val newZipper = Zipper(null, Nil, trees => q"..$trees")
          val whiletree = successors.head.markEmit(newZipper, seen, subgraph).result
          val z2 = z1.append(whiletree)
          successors.last.markEmit(z2, seen, subgraph)
        } else sys.error(s"Number of successors for <$tree>: ${successors.length}")
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.parent
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = WhileEnd(nch, uid)
    }

    case class Block(tree: Tree, chain: Chain, uid: Long) extends Node {
      override def code = q""
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        val q"{ ..$stats }" = tree
        val z1 = z.descend(trees => q"{ ..$trees }")
        successors.head.markEmit(z1, seen, subgraph)
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.descend
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = Block(tree, nch, uid)
    }

    case class BlockEnd(chain: Chain, uid: Long) extends Node {
      val tree: Tree = q""
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        if (successors.length == 1) {
          if (successors.head.isEmptyAtReturn) {
            val exittree = genExit(this, subgraph)
            z.append(exittree)
          } else {
            val z1 = z.ascend
            successors.head.markEmit(z1, seen, subgraph)
          }
        } else if (successors.length == 0) {
          // do nothing
          val exittree = genExit(this, subgraph)
          z.append(exittree)
        } else sys.error(s"Multiple successors for <$tree>.")
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.parent
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      override def isEmptyAtReturn = {
        if (successors.length == 0) true
        else successors.head.isEmptyAtReturn
      }
      def copyWithoutSuccessors(nch: Chain) = BlockEnd(nch , uid)
    }

    case class ApplyCoroutine(tree: Tree, chain: Chain, uid: Long) extends Node {
      val (co, args) = tree match {
        case q"$_ val $_: $_ = $co.apply(..$args)" => (co, args)
        case q"$_ val $_: $_ = $co.apply[..$_](..$args)($_)" => (co, args)
      }
      def coroutine: Tree = co
      override def code: Tree = {
        q"""
          $co

          ..$args
        """
      }
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        val exittree = genCoroutineCall(co, args, subgraph)
        z.append(exittree)
      }
      override def updateBlock()(implicit table: Table) {
        super.updateBlock()
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.withDecl(tree, false)
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        this.addSuccessorsToNodeFront(ctx)
        ctx.exitPoints(subgraph)(nthis) = this.successors.head.uid

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = ApplyCoroutine(tree, nch, uid)
      override def stackVars(sub: SubCfg) =
        tree.symbol :: storePointVarsInChain(sub).map(_._1)
      def genCoroutineCall(
        co: Tree, args: List[Tree], subgraph: SubCfg
      )(implicit table: Table): Tree = {
        val coelemtpe = coroutineElemType(co.tpe)
        val cparam = table.names.coroutineParam
        val savestate = genSaveState(subgraph)
        val untypedco = table.untyper.untypecheck(co)
        val untypedargs = for (a <- args) yield table.untyper.untypecheck(a)
        q"""
          import scala.coroutines.Permission.canCall
          ..$savestate
          $untypedco.$$push($cparam.asInstanceOf[Coroutine[$coelemtpe]], ..$untypedargs)
          $cparam.$$target = $cparam
        """
      }
    }

    case class YieldVal(tree: Tree, chain: Chain, uid: Long) extends Node {
      override def code = {
        tree match {
          case q"$_ val $_: $_ = $qual.yieldval[$_]($x)" if isCoroutinesPkg(qual) => x
          case q"$_ var $_: $_ = $qual.yieldval[$_]($x)" if isCoroutinesPkg(qual) => x
        }
      }
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        val x = tree match {
          case q"$_ val $_: $_ = $qual.yieldval[$_]($x)" if isCoroutinesPkg(qual) => x
          case q"$_ var $_: $_ = $qual.yieldval[$_]($x)" if isCoroutinesPkg(qual) => x
        }
        val cparam = table.names.coroutineParam
        val savestate = genSaveState(subgraph)
        val exittree = q"""
          ..$savestate
          $$assignresult($cparam, ${table.untyper.untypecheck(x)})
          return
        """
        val z1 = z.append(exittree)
        z1
      }
      override def stackVars(sub: SubCfg) = storePointVarsInChain(sub).map(_._1)
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nthis = this.copyWithoutSuccessors(prevchain)
        seen(this) = nthis
        nthis.updateBlock()

        this.addSuccessorsToNodeFront(ctx)
        ctx.exitPoints(subgraph)(nthis) = this.successors.head.uid

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = YieldVal(tree, nch, uid)
    }

    case class YieldTo(tree: Tree, chain: Chain, uid: Long) extends Node {
      override def code = {
        tree match {
          case q"$_ val $_: $_ = $qual.yieldto[$_]($x)" if isCoroutinesPkg(qual) => x
          case q"$_ var $_: $_ = $qual.yieldto[$_]($x)" if isCoroutinesPkg(qual) => x
        }
      }
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        val co = tree match {
          case q"$_ val $_: $_ = $qual.yieldto[$_]($x)" if isCoroutinesPkg(qual) => x
          case q"$_ var $_: $_ = $qual.yieldto[$_]($x)" if isCoroutinesPkg(qual) => x
        }
        val untypedco = table.untyper.untypecheck(co)
        val cparam = table.names.coroutineParam
        val savestate = genSaveState(subgraph)
        val exittree = q"""
          ..$savestate
          $cparam.$$target = $untypedco.asInstanceOf[Coroutine[${table.returnType}]]
          return
        """
        z.append(exittree)
      }
      override def stackVars(sub: SubCfg) = storePointVarsInChain(sub).map(_._1)
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nthis = this.copyWithoutSuccessors(prevchain)
        seen(this) = nthis
        nthis.updateBlock()

        this.addSuccessorsToNodeFront(ctx)
        ctx.exitPoints(subgraph)(nthis) = this.successors.head.uid

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = YieldTo(tree, nch, uid)
    }

    abstract class Statement extends Node {
      def emit(z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg)(
        implicit cc: CanCall, table: Table
      ): Zipper = {
        // inside the control-flow-construct, normal statement
        if (successors.length == 1) {
          if (successors.head.isEmptyAtReturn) {
            val exittree = genExit(this, subgraph)
            z.append(exittree)
          } else {
            val z1 = z.append(table.untyper.untypecheck(tree))
            successors.head.markEmit(z1, seen, subgraph)
          }
        } else if (successors.length == 0) {
          val exittree = genExit(this, subgraph)
          z.append(exittree)
        } else sys.error(s"Multiple successors for <$tree>.")
      }
    }

    case class DefaultStatement(tree: Tree, chain: Chain, uid: Long)
    extends Statement {
      override def updateBlock()(implicit table: Table) {
        super.updateBlock()
        tree match {
          case q"$x = $v" if table.contains(x.symbol) =>
            chain.block.assignments(x.symbol) = table(x.symbol)
          case _ =>
        }
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nthis = this.copyWithoutSuccessors(prevchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = DefaultStatement(tree, nch, uid)
    }

    case class Decl(tree: Tree, chain: Chain, uid: Long)
    extends Statement {
      override def value = q"()"
      override def code: Tree = tree match {
        case q"$_ val $_: $_ = $rhs" => rhs
        case q"$_ var $_: $_ = $rhs" => rhs
      }
      override def updateBlock()(implicit table: Table) {
        super.updateBlock()
        chain.block.decls(tree.symbol) = table(tree.symbol)
      }
      def extract(
        prevchain: Chain, seen: mutable.Map[Node, Node], ctx: ExtractSubgraphContext,
        subgraph: SubCfg
      )(implicit table: Table): Node = {
        val nchain = prevchain.withDecl(tree, false)
        val nthis = this.copyWithoutSuccessors(nchain)
        seen(this) = nthis
        nthis.updateBlock()

        for (s <- this.successors) {
          if (!seen.contains(s)) {
            s.extract(nthis.chain, seen, ctx, subgraph)
          }
          nthis.successors += seen(s)
        }

        nthis
      }
      def copyWithoutSuccessors(nch: Chain) = Decl(tree, nch, uid)
    }
  }

  class Cfg(val start: Node, val subgraphs: Map[Node, SubCfg])(implicit table: Table) {
    val stackVars = mutable.Set[Symbol]()

    def storedValVars = table.valvars.filter(kv => stackVars.contains(kv._1))

    def storedRefVars = table.refvars.filter(kv => stackVars.contains(kv._1))

    def initializeStackPositions() {
      // find all stack variables
      stackVars ++= (for {
        sub <- subgraphs.values
        n <- sub.start.dfs
        s <- n.stackVars(sub)
      } yield s)
      stackVars ++= table.vars.filter(_._2.isArg).keys

      // stack positions
      def compute(vars: Map[Symbol, VarInfo]) {
        var poscount = 0
        for ((sym, info) <- vars if stackVars.contains(sym)) {
          info.stackpos = (poscount, info.width)
          poscount += info.width
        }
      }

      compute(table.refvars)
      compute(table.valvars)
    }
    initializeStackPositions()
  }

  class SubCfg(val uid: Long) {
    val exitSubgraphs = mutable.LinkedHashMap[Node, SubCfg]()
    var start: Node = _
    val all = mutable.LinkedHashMap[Long, Node]()
    val childBlocks = mutable.LinkedHashMap[Block, Set[Block]]()

    def initializeBlocks() {
      val chains = start.dfs.map(_.chain).map(_.ancestors).flatten.toSet
      childBlocks ++= chains
        .filter(_.parent != null)
        .groupBy(_.parent)
        .groupBy(_._1.block)
        .map { case (block, chainmaps) =>
          (block, chainmaps.flatMap(_._2.map(_.block)).toSet)
        }
      for (c <- chains) if (!childBlocks.contains(c.block)) childBlocks(c.block) = Set()
    }

    val isOccurringInBlockDescendants: Cache._2[Symbol, Block, Boolean] = cached {
      (s, b) =>
      b.occurrences.contains(s) ||
        childBlocks(b).exists(isOccurringInBlockDescendants(s, _))
    }

    val isAssignedInBlockDescendants: Cache._2[Symbol, Block, Boolean] = cached {
      (s, b) =>
      b.assignments.contains(s) ||
        childBlocks(b).exists(isAssignedInBlockDescendants(s, _))
    }

    val isLoadedInReachableSubgraphs: Cache._2[Node, Symbol, Boolean] = cached {
      (n, s) =>
      def isLoaded(sub: SubCfg, seen: mutable.Set[SubCfg]): Boolean = {
        if (seen(sub)) false else {
          seen += sub
          val startChain = sub.start.chain.chainForDecl(s).get
          sub.mustLoadVar(s, startChain) ||
            sub.exitSubgraphs
              .filter(_._1.chain.isDescendantOf(startChain))
              .exists(t => isLoaded(t._2, seen))
        }
      }
      isLoaded(exitSubgraphs(n), mutable.Set())
    }

    val declarationBlockFrom: Cache._2[Symbol, Chain, Block] = cached {
      (s, chain) =>
      chain.ancestors.find(_.decls.toMap.contains(s)).get.block
    }

    val mustStoreVar: Cache._2[Node, Symbol, Boolean] = cached {
      (n, sym) =>
      val chain = n.chain
      val block = declarationBlockFrom(sym, chain)
      val isVisible = chain.contains(sym)
      val isAssigned = isAssignedInBlockDescendants(sym, block)
      val isDeclared = chain.isDeclaredInAncestors(sym)
      val isNeeded = isLoadedInReachableSubgraphs(n, sym)
      isVisible && (isAssigned || isDeclared) && isNeeded
    }

    val mustLoadVar: Cache._2[Symbol, Chain, Boolean] = cached {
      (sym, chain) =>
      val isVisible = chain.contains(sym)
      val isOccurring = isOccurringInBlockDescendants(sym, chain.block)
      isVisible && isOccurring
    }

    def emit()(implicit table: Table): Tree = {
      def findStart(chain: Chain): Zipper = {
        var z = {
          if (chain.parent == null) Zipper(null, Nil, trees => q"..$trees")
          else findStart(chain.parent).descend(trees => q"..$trees")
        }
        for ((sym, info) <- chain.decls) {
          if (mustLoadVar(sym, chain)) {
            val cparam = table.names.coroutineParam
            val stack = info.stackname
            val decodedget = info.loadTree(q"$cparam")
            val valdef = info.origtree match {
              case q"$mods val $name: $tpt = $_" =>
                q"$mods val $name: $tpt = $decodedget"
              case q"$mods var $name: $tpt = $_" =>
                q"$mods var $name: $tpt = $decodedget"
            }
            z = z.append(valdef)
          }
        }
        z
      }
      val startzipper = findStart(start.chain)
      val bodyzipper = start.emitCode(startzipper, this)
      val body = bodyzipper.result
      body
    }
  }

  def genControlFlowGraph(args: List[Tree], body: Tree, tpt: Tree)(
    implicit table: Table
  ): Cfg = {
    def traverse(t: Tree, ch: Chain): (Node, Node) = {
      t match {
        case q"$_ val $_: $_ = $qual.yieldval[$_]($_)" if isCoroutinesPkg(qual) =>
          val nch = ch.withDecl(t, false)
          val n = Node.YieldVal(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case q"$_ var $_: $_ = $qual.yieldval[$_]($_)" if isCoroutinesPkg(qual) =>
          val nch = ch.withDecl(t, false)
          val n = Node.YieldVal(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case q"$_ val $_: $_ = $qual.yieldto[$_]($_)" if isCoroutinesPkg(qual) =>
          val nch = ch.withDecl(t, false)
          val n = Node.YieldTo(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case q"$_ var $_: $_ = $qual.yieldto[$_]($_)" if isCoroutinesPkg(qual) =>
          val nch = ch.withDecl(t, false)
          val n = Node.YieldTo(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case ValDecl(t @ q"$_ val $_ = $c.apply(..$_)")
          if isCoroutineBlueprintMarker(c.tpe) =>
          val nch = ch.withDecl(t, false)
          val n = Node.ApplyCoroutine(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case ValDecl(t @ q"$_ var $_ = $c.apply(..$_)")
          if isCoroutineBlueprintMarker(c.tpe) =>
          val nch = ch.withDecl(t, false)
          val n = Node.ApplyCoroutine(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case ValDecl(t @ q"$_ val $_ = $c.apply[..$_](..$_)($_)")
          if isCoroutineBlueprintSugar(c.tpe) =>
          val nch = ch.withDecl(t, false)
          val n = Node.ApplyCoroutine(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case ValDecl(t @ q"$_ var $_ = $c.apply[..$_](..$_)($_)")
          if isCoroutineBlueprintSugar(c.tpe) =>
          val nch = ch.withDecl(t, false)
          val n = Node.ApplyCoroutine(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case ValDecl(t) =>
          val nch = ch.withDecl(t, false)
          val n = Node.Decl(t, ch, table.newNodeUid())
          val u = Node.DefaultStatement(q"()", nch, table.newNodeUid())
          n.successors += u
          (n, u)
        case q"return $_" =>
          c.abort(t.pos, "Return statements not allowed inside coroutines.")
        case q"if ($cond) $thenbranch else $elsebranch" =>
          val endnode = Node.IfEnd(ch, table.newNodeUid())
          val ifnode = Node.If(endnode.uid, t, ch, table.newNodeUid())
          def addBranch(branch: Tree) {
            val nestedchain = ch.descend
            val (childhead, childlast) = traverse(branch, nestedchain)
            ifnode.successors += childhead
            if (childlast.tree.tpe =:= typeOf[Unit]) {
              val unode = Node.DefaultStatement(q"()", nestedchain, table.newNodeUid())
              childlast.successors += unode
              unode.successors += endnode
            } else {
              childlast.successors += endnode
            }
          }
          addBranch(thenbranch)
          addBranch(elsebranch)
          (ifnode, endnode)
        case q"while ($cond) $body" =>
          val whilenode = Node.While(t, ch, table.newNodeUid())
          val endnode = Node.WhileEnd(ch, table.newNodeUid())
          val nestedchain = ch.descend
          val (childhead, childlast) = traverse(body, nestedchain)
          whilenode.successors += childhead
          childlast.successors += endnode
          endnode.successors += whilenode
          whilenode.successors += endnode
          (whilenode, endnode)
        case q"{ ..$stats }" if stats.nonEmpty && stats.tail.nonEmpty =>
          val blocknode = Node.Block(t, ch, table.newNodeUid())
          val endnode = Node.BlockEnd(ch, table.newNodeUid())
          val nestedchain = ch.descend
          val (first, childlast) = traverse(stats.head, nestedchain)
          var current = childlast
          var currchain = current.chain
          for (stat <- stats.tail) {
            val (childhead, childlast) = traverse(stat, currchain)
            current.successors += childhead
            current = childlast
            currchain = current.chain
          }
          blocknode.successors += first
          current.successors += endnode
          (blocknode, endnode)
        case _ =>
          val n = Node.DefaultStatement(t, ch, table.newNodeUid())
          (n, n)
      }
    }

    // add arguments to symbol table
    val bodyChain = args.foldLeft(table.topChain: Chain) { (ch, t) =>
      val q"$_ val $name: $_ = $_" = t
      ch.withDecl(t, true)
    }

    // traverse tree to construct CFG and extract local variables
    val (head, last) = traverse(body, bodyChain)
    println(head.prettyPrint)

    // extract subgraphs in the control flow graph
    val subgraphs = extractSubgraphs(head, tpt)

    // construct graph object
    val cfg = new Cfg(head, subgraphs)

    cfg
  }

  class ExtractSubgraphContext(val rettpt: Tree) {
    val subgraphs = mutable.LinkedHashMap[Node, SubCfg]()
    val exitPoints = mutable.LinkedHashMap[SubCfg, mutable.LinkedHashMap[Node, Long]]()
    val seenEntryPoints = mutable.LinkedHashSet[Node]()
    val nodefront = mutable.Queue[Node]()
  }

  def extractSubgraphs(start: Node, rettpt: Tree)(
    implicit table: Table
  ): Map[Node, SubCfg] = {
    val ctx = new ExtractSubgraphContext(rettpt)
    ctx.seenEntryPoints += start
    ctx.nodefront.enqueue(start)

    // as long as there are more nodes on the expansion front, extract them
    while (ctx.nodefront.nonEmpty) {
      val subgraph = new SubCfg(table.newSubgraphUid())
      val node = ctx.nodefront.dequeue()
      ctx.exitPoints(subgraph) = mutable.LinkedHashMap[Node, Long]()
      subgraph.start = node.extract(
        node.chain.copyWithoutBlocks, mutable.Map(), ctx, subgraph)
      subgraph.all ++= subgraph.start.dfs.map(n => n.uid -> n)
      subgraph.initializeBlocks()
      ctx.subgraphs(node) = subgraph
    }

    // assign respective subgraph reference to each exit point node
    val startPoints = ctx.subgraphs.map(s => s._2.start.uid -> s._2).toMap
    for ((subgraph, exitMap) <- ctx.exitPoints; (node, nextUid) <- exitMap) {
      subgraph.exitSubgraphs(node) = startPoints(nextUid)
    }

    println(ctx.subgraphs
      .map({ case (k, v) => 
        v.start.prettyPrint + "\n"
      })
      .zipWithIndex.map(t => s"\n${t._2}:\n${t._1}")
      .mkString("\n"))
    ctx.subgraphs
  } 
}
