package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Generates control flow graphs, and converts CFG nodes to ASTs.
 */
trait ControlFlowGraph[C <: Context] {
  self: Analyzer[C] =>

  val c: C

  import c.universe._

  private sealed trait CanEmit

  private object Permissions {
    implicit object canEmit extends CanEmit
  }

  abstract class Node {
    var successors: List[Node] = Nil

    val uid: Long

    val tree: Tree

    def chain: Chain

    def copyWithoutSuccessors: Node

    def code: Tree = tree

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
      this.markEmit(z, seen, subgraph)
    }

    final def markEmit(
      z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
    )(implicit t: Table): Zipper = {
      import Permissions.canEmit
      if (!seen(this)) {
        seen += this
        this.emit(z, seen, subgraph)
      } else z
    }

    def emit(
      z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
    )(implicit ce: CanEmit, t: Table): Zipper

    protected def genSaveState(
      chain: Chain, subgraph: SubCfg
    )(implicit t: Table): List[Tree] = {
      val cparam = t.names.coroutineParam
      // store state for non-val variables in scope
      val stacksets = for {
        (sym, info) <- chain.allvars
        if subgraph.mustStoreVar(sym)
      } yield {
        val stack = info.stackname
        val pos = info.stackpos
        val encodedval = info.encodeLong(q"${info.name}")
        q"scala.coroutines.common.Stack.set($cparam.$stack, $pos, $encodedval)"
      }
      // update pc state
      val pc = subgraph.exitSubgraphs(this).uid
      val pcstackset = q"""
        scala.coroutines.common.Stack.update($cparam.pcstack, $pc.toShort)
      """
      pcstackset :: stacksets.toList
    }

    protected def genExit(
      n: Node, subgraph: SubCfg
    )(implicit t: Table): Tree = {
      val cparam = t.names.coroutineParam
      val untypedTree = t.untyper.untypecheck(n.code)
      q"""
        pop($cparam)
        if (scala.coroutines.common.Stack.isEmpty($cparam.costack)) {
          $cparam.result = $untypedTree
        } else {
          import scala.coroutines.Permission.canCall
          $cparam.target = $cparam
          scala.coroutines.common.Stack.top($cparam.costack)
            .returnvalue($cparam, $untypedTree)
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
        text.append(
          s"$prefix|-> $count: ${n.getClass.getName.dropWhile(_ != '$')}($treerepr)\n")
        count += 1
        def emitChild(c: Node, newPrefix: String) {
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

  object Node {
    case class If(term: IfTerm, tree: Tree, chain: Chain, uid: Long) extends Node {
      override def code = {
        val q"if ($cond) $_ else $_" = tree
        cond
      }
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        val q"if ($cond) $_ else $_" = tree
        val newZipper = Zipper(null, Nil, trees => q"..$trees")
        val elsen = this.successors(1)
        val thenn = this.successors(0)
        val elsebranch = elsen.markEmit(newZipper, mutable.Set(term), subgraph).result
        val thenbranch = thenn.markEmit(newZipper, mutable.Set(term), subgraph).result
        val untypedcond = table.untyper.untypecheck(cond)
        val iftree = q"if ($untypedcond) $thenbranch else $elsebranch"
        val z1 = z.append(iftree)
        term.markEmit(z1, seen, subgraph)
      }
      def copyWithoutSuccessors = If(term, tree, chain, uid)
    }

    case class IfTerm(chain: Chain, uid: Long) extends Node {
      val tree: Tree = q"()"
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        if (successors.length == 1) {
          val z1 = z.ascend
          successors.head.markEmit(z1, seen, subgraph)
        } else if (successors.length == 0) {
          // do nothing
          z
        } else sys.error(s"Multiple successors for <$tree>.")
      }
      def copyWithoutSuccessors = IfTerm(chain, uid)
    }

    case class While(term: WhileTerm, tree: Tree, chain: Chain, uid: Long)
    extends Node {
      override def code = {
        val q"while ($cond) $_" = tree
        cond
      }
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        val q"while ($cond) $body" = tree
        val untypedcond = table.untyper.untypecheck(cond)
        val z1 = z.descend(trees => q"while ($untypedcond) ..$trees")
        successors(0).markEmit(z1, seen, subgraph)
      }
      def copyWithoutSuccessors = While(term, tree, chain, uid)
    }

    case class WhileTerm(chain: Chain, uid: Long) extends Node {
      val tree: Tree = q"()"
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        if (successors.length == 1) {
          // do nothing
          val z1 = z.ascend
          successors.head.markEmit(z1, seen, subgraph)
        } else if (successors.length == 2) {
          val z1 = z.ascend
          val z2 = successors.last.markEmit(z1, mutable.Set(this), subgraph)
          successors.head.markEmit(z2, seen, subgraph)
        } else sys.error(s"Number of successors for <$tree>: ${successors.length}")
      }
      def copyWithoutSuccessors = WhileTerm(chain, uid)
    }

    abstract class CoroutineCall extends Node {
      def genCoroutineCall(
        co: Tree, args: List[Tree], chain: Chain, subgraph: SubCfg
      )(implicit table: Table): Tree = {
        val cparam = table.names.coroutineParam
        val savestate = genSaveState(chain, subgraph)
        val untypedArgs = for (a <- args) yield table.untyper.untypecheck(a)
        q"""
          import scala.coroutines.Permission.canCall
          ..$savestate
          $co.push($cparam, ..$untypedArgs)
          $cparam.target = $cparam
        """
      }
    }

    case class ValCoroutineCall(tree: Tree, chain: Chain, uid: Long)
    extends Node.CoroutineCall {
      def coroutine: Tree = {
        val q"$_ val $_: $_ = $co.apply(..$_)" = tree
        co
      }
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        val q"$_ val $_: $_ = $co.apply(..$args)" = tree
        val termtree = genCoroutineCall(co, args, chain, subgraph)
        z.append(termtree)
      }
      def copyWithoutSuccessors = ValCoroutineCall(tree, chain, uid)
    }

    case class YieldVal(tree: Tree, chain: Chain, uid: Long) extends Node {
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        val q"coroutines.this.`package`.yieldval[$_]($x)" = tree
        val cparam = table.names.coroutineParam
        val savestate = genSaveState(chain, subgraph)
        val termtree = q"""
          ..$savestate
          $cparam.result = ${table.untyper.untypecheck(x)}
          return
        """
        z.append(termtree)
      }
      def copyWithoutSuccessors = YieldVal(tree, chain, uid)
    }

    case class YieldTo(tree: Tree, chain: Chain, uid: Long) extends Node {
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        val q"coroutines.this.`package`.yieldto[$_]($co)" = tree
        val cparam = table.names.coroutineParam
        val savestate = genSaveState(chain, subgraph)
        val termtree = q"""
          ..$savestate
          $cparam.target = ${table.untyper.untypecheck(co)}
          return
        """
        z.append(termtree)
      }
      def copyWithoutSuccessors = YieldTo(tree, chain, uid)
    }

    case class Statement(tree: Tree, chain: Chain, uid: Long) extends Node {
      def emit(
        z: Zipper, seen: mutable.Set[Node], subgraph: SubCfg
      )(implicit ce: CanEmit, table: Table): Zipper = {
        // inside the control-flow-construct, normal statement
        if (successors.length == 1) {
          val z1 = z.append(table.untyper.untypecheck(tree))
          successors.head.markEmit(z1, seen, subgraph)
        } else if (successors.length == 0) {
          if (!subgraph.exitSubgraphs.contains(this)) {
            // if this was the last expression in the original graph,
            // then pop the stack and store expression to the return value position
            val termtree = genExit(this, subgraph)
            z.append(termtree)
          } else {
            z
          }
        } else sys.error(s"Multiple successors for <$tree>.")
      }
      def copyWithoutSuccessors = Statement(tree, chain, uid)
    }
  }

  class Cfg(val start: Node) {
    val subgraphs = mutable.Map[Node, SubCfg]()
  }

  class SubCfg(val uid: Long) {
    val referencedVars = mutable.LinkedHashMap[Symbol, VarInfo]()
    val declaredVars = mutable.LinkedHashMap[Symbol, VarInfo]()
    val exitSubgraphs = mutable.LinkedHashMap[Node, SubCfg]()
    var start: Node = _
    def usesVar(sym: Symbol) = referencedVars.contains(sym)
    def declaresVar(sym: Symbol) = declaredVars.contains(sym)
    def mustStoreVar(sym: Symbol) = {
      usesVar(sym) && (sym.asTerm.isVar || declaresVar(sym))
    }
  }

  def genControlFlowGraph(tpt: Tree)(implicit table: Table): Cfg = {
    def traverse(t: Tree, ch: Chain): (Node, Node) = {
      t match {
        case q"coroutines.this.`package`.yieldval[$_]($_)" =>
          val n = Node.YieldVal(t, ch, table.newNodeUid())
          (n, n)
        case q"coroutines.this.`package`.yieldto[$_]($_)" =>
          val n = Node.YieldTo(t, ch, table.newNodeUid())
          (n, n)
        case ValDecl(t @ q"$_ val $_ = $co.apply($_)") if isCoroutineDefType(co.tpe) =>
          ch.addVar(t, false)
          val n = Node.ValCoroutineCall(t, ch, table.newNodeUid())
          (n, n)
        case ValDecl(t @ q"$_ var $_ = $co.apply($_)") if isCoroutineDefType(co.tpe) =>
          ch.addVar(t, false)
          val n = Node.ValCoroutineCall(t, ch, table.newNodeUid())
          (n, n)
        case ValDecl(t) =>
          ch.addVar(t, false)
          val n = Node.Statement(t, ch, table.newNodeUid())
          (n, n)
        case q"return $_" =>
          c.abort(t.pos, "Return statements not allowed inside coroutines.")
        case q"if ($cond) $thenbranch else $elsebranch" =>
          val termnode = Node.IfTerm(ch, table.newNodeUid())
          val ifnode = Node.If(termnode, t, ch, table.newNodeUid())
          def addBranch(branch: Tree) {
            val nestedchain = ch.newChain(t)
            val (childhead, childlast) = traverse(branch, nestedchain)
            ifnode.successors ::= childhead
            childlast.tree match {
              case ValDecl(_) =>
                val endnode = Node.Statement(q"()", nestedchain, table.newNodeUid())
                childlast.successors ::= endnode
                endnode.successors ::= termnode
              case _ =>
                childlast.successors ::= termnode
            }
          }
          addBranch(thenbranch)
          addBranch(elsebranch)
          (ifnode, termnode)
        case q"while ($cond) $body" =>
          val termnode = Node.WhileTerm(ch, table.newNodeUid())
          val whilenode = Node.While(termnode, t, ch, table.newNodeUid())
          val nestedchain = ch.newChain(t)
          val (childhead, childlast) = traverse(body, nestedchain)
          whilenode.successors ::= childhead
          childlast.successors ::= termnode
          termnode.successors ::= whilenode
          (whilenode, termnode)
        case q"{ ..$stats }" if stats.nonEmpty && stats.tail.nonEmpty =>
          val nestedchain = ch.newChain(t)
          val (first, childlast) = traverse(stats.head, nestedchain)
          var current = childlast
          for (stat <- stats.tail) {
            val (childhead, childlast) = traverse(stat, nestedchain)
            current.successors ::= childhead
            current = childlast
          }
          (first, current)
        case _ =>
          val n = Node.Statement(t, ch, table.newNodeUid())
          (n, n)
      }
    }

    val lambda = table.lambda
    val (args, body) = lambda match {
      case q"(..$args) => $body" => (args, body)
      case _ => c.abort(lambda.pos, "The coroutine takes a single function literal.")
    }

    for (t <- args) {
      val q"$_ val $name: $_ = $_" = t
      table.topChain.addVar(t, true)
    }

    // traverse tree to construct CFG and extract local variables
    val (head, last) = traverse(body, table.topChain)
    println(head.prettyPrint)

    // extract subgraphs in the control flow graph
    val subgraphs = extractSubgraphs(head, tpt)

    // construct graph object
    val cfg = new Cfg(head)
    cfg.subgraphs ++= subgraphs
    cfg
  }

  def extractSubgraphs(
    start: Node, rettpt: Tree
  )(implicit table: Table): Map[Node, SubCfg] = {
    val subgraphs = mutable.LinkedHashMap[Node, SubCfg]()
    val exitPoints = mutable.Map[SubCfg, mutable.Map[Node, Long]]()
    val seenEntries = mutable.Set[Node]()
    val nodefront = mutable.Queue[Node]()
    seenEntries += start
    nodefront.enqueue(start)

    def extract(
      n: Node, seen: mutable.Map[Node, Node], subgraph: SubCfg
    ): Node = {
      // duplicate and mark current node as seen
      val current = n.copyWithoutSuccessors
      seen(n) = current

      // detect referenced and declared stack variables
      for (t <- n.code) {
        t match {
          case q"$_ val $_: $_ = $_" =>
            subgraph.declaredVars(t.symbol) = table(t.symbol)
          case q"$_ var $_: $_ = $_" =>
            subgraph.declaredVars(t.symbol) = table(t.symbol)
          case _ =>
            if (table.contains(t.symbol)) {
              subgraph.referencedVars(t.symbol) = table(t.symbol)
            }
        }
      }

      // check for termination condition
      def addToNodeFront() {
        // add successors to node front
        for (s <- n.successors) if (!seenEntries(s)) {
          seenEntries += s
          nodefront.enqueue(s)
        }
      }
      def addCoroutineInvocationToNodeFront(co: Tree) {
        val coroutinetpe = coroutineTypeFor(rettpt.tpe)
        if (!(co.tpe <:< coroutinetpe)) {
          c.abort(co.pos,
            s"Coroutine invocation site has invalid return type.\n" +
            s"required: $coroutinetpe\n" +
            s"found:    ${co.tpe} (with underlying type ${co.tpe.widen})")
        }
        addToNodeFront()
      }
      n.tree match {
        case q"coroutines.this.`package`.yieldval[$_]($_)" =>
          addToNodeFront()
          exitPoints(subgraph)(current) = n.successors.head.uid
        case q"coroutines.this.`package`.yieldto[$_]($_)" =>
          addToNodeFront()
          exitPoints(subgraph)(current) = n.successors.head.uid
        case q"$_ val $_ = $co.apply(..$args)" if isCoroutineDefType(co.tpe) =>
          addCoroutineInvocationToNodeFront(co)
          exitPoints(subgraph)(current) = n.successors.head.uid
        case _ =>
          // traverse successors
          for (s <- n.successors) {
            if (!seen.contains(s)) {
              extract(s, seen, subgraph)
            }
            current.successors ::= seen(s)
          }
      }
      current
    }

    // as long as there are more nodes on the expansion front, extract them
    while (nodefront.nonEmpty) {
      val subgraph = new SubCfg(table.newSubgraphUid())
      val node = nodefront.dequeue()
      exitPoints(subgraph) = mutable.Map[Node, Long]()
      subgraph.start = extract(node, mutable.Map(), subgraph)
      subgraphs(node) = subgraph
    }

    // assign respective subgraph reference to each exit point node
    val startPoints = subgraphs.map(s => s._2.start.uid -> s._2).toMap
    for ((subgraph, exitMap) <- exitPoints; (node, nextUid) <- exitMap) {
      subgraph.exitSubgraphs(node) = startPoints(nextUid)
    }

    println(subgraphs
      .map({ case (k, v) => 
        "[" + v.referencedVars.keys.mkString(", ") + "]\n" + v.start.prettyPrint + "\n"
      })
      .zipWithIndex.map(t => s"\n${t._2}:\n${t._1}")
      .mkString("\n"))
    subgraphs
  }
}
