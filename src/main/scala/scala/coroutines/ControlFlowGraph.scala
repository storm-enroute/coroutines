package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Generates control flow graphs, and converts CFG nodes to ASTs.
 */
trait ControlFlowGraph[C <: Context] {
  self: Synthesizer[C] with Analyzer[C] =>

  val c: Context

  import c.universe._

  private sealed trait CanEmit

  private object Permissions {
    implicit object canEmit extends CanEmit
  }

  class Node(
    val tree: Tree,
    val ctrlflowtree: Option[Tree],
    val chain: Chain
  ) {
    var successors: List[Node] = Nil

    def singleSuccessor: Option[Node] = {
      if (successors.size == 1) Some(successors.head)
      else None
    }

    final def emitCode(z: Zipper): Zipper = {
      val seen = mutable.Set[Node]()
      this.markAndEmitTree(z, seen)
    }

    final def markAndEmitTree(z: Zipper, seen: mutable.Set[Node]): Zipper = {
      import Permissions.canEmit
      if (!seen(this)) {
        seen += this
        this.emit(z, seen)
      } else z
    }

    def emit(z: Zipper, seen: mutable.Set[Node])(implicit ce: CanEmit): Zipper = {
      ctrlflowtree match {
        case None =>
          // inside the control-flow-construct, normal statement
          val z1 = z.append(table.untyper.untypecheck(tree))
          singleSuccessor match {
            case Some(sn) => sn.markAndEmitTree(z1, seen)
            case None => z1
          }
        case Some(cftree) if cftree eq tree =>
          // node marks the start of a control-flow-construct
          cftree match {
            case q"if ($cond) $_ else $_" =>
              val newZipper = Zipper(null, Nil, trees => q"..$trees")
              val elsenode = this.successors(0)
              val thennode = this.successors(1)
              val elsebranch = elsenode.markAndEmitTree(newZipper, seen).root.result
              val thenbranch = thennode.markAndEmitTree(newZipper, seen).root.result
              val untypedcond = table.untyper.untypecheck(cond)
              val iftree = q"if ($untypedcond) $thenbranch else $elsebranch"
              val z1 = z.append(iftree)
              z1
            case _ =>
              sys.error("Unknown control flow construct: $cftree")
          }
        case Some(cftree) if cftree ne tree =>
          // node marks the end of a control-flow-construct
          val z1 = this.backwardSuccessor.markAndEmitTree(z, seen)
          val z2 = this.forwardSuccessor.markAndEmitTree(z1, seen)
          z2
      }
    }

    def forwardSuccessor: Node = ctrlflowtree match {
      case Some(q"if ($_) $_ else $_") =>
        successors.head
      case None =>
        sys.error(s"Cannot compute forward node for <$tree>.")
    }

    def backwardSuccessor: Node = ctrlflowtree match {
      case Some(q"if ($_) $_ else $_") =>
        new Node(q"()", None, chain)
      case None =>
        sys.error(s"Cannot compute backward node for <$tree>.")
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
        text.append(s"$prefix|-> $count: Node($treerepr)\n")
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
    def copyNoSuccessors(n: Node) =
      new Node(n.tree, n.ctrlflowtree, n.chain)
  }
}
