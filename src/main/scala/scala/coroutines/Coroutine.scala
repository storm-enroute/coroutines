package scala.coroutines



import scala.annotation.tailrec
import scala.collection._
import scala.coroutines.common.Stack
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



class Coroutine[@specialized T] {
  import Coroutine._
  private[coroutines] var costackptr = 0
  private[coroutines] var costack = new Array[Definition[T]](INITIAL_CO_STACK_SIZE)
  private[coroutines] var pcstackptr = 0
  private[coroutines] var pcstack = new Array[Short](INITIAL_CO_STACK_SIZE)
  private[coroutines] var target: Coroutine[T] = null
  private[coroutines] var result: T = null.asInstanceOf[T]

  final def push(cd: Coroutine.Definition[T]) {
    Stack.push(costack, cd)
    Stack.push(pcstack, 0.toShort)
    cd.push(this)
  }

  final def pop() {
    Stack.pop(pcstack)
    val cd = Stack.pop(costack)
    cd.pop(this)
  }

  @tailrec
  private[coroutines] final def enter(): T = {
    val cd = Stack.top(costack)
    cd.enter(this)
    if (target ne null) {
      val nc = target
      target = null
      nc.enter()
    } else result
  }

  def apply(): T = enter()
}


object Coroutine {
  private[coroutines] val INITIAL_CO_STACK_SIZE = 4

  abstract class Definition[T] {
    def push(c: Coroutine[T]): Unit
    def pop(c: Coroutine[T]): Unit
    def enter(c: Coroutine[T]): Unit
  }

  def transform(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).transform(f)
  }

  private[coroutines] class Synthesizer[C <: Context](val c: C) {
    import c.universe._

    case class VarInfo(position: Int)

    type VarMap = Map[Symbol, VarInfo]

    private def inferReturnType(body: Tree): Tree = {
      // return type must correspond to the return type of the function literal
      val rettpe = body.tpe

      // return type is the lub of the function return type and yield argument types
      def isCoroutines(q: Tree) = q match {
        case q"coroutines.this.`package`" => true
        case t => false
      }
      val constraintTpes = body.collect {
        case q"$qual.yieldval[$tpt]($v)" if isCoroutines(qual) => tpt.tpe
        case q"$qual.yieldto[$tpt]($f)" if isCoroutines(qual) => tpt.tpe
      }
      tq"${lub(rettpe :: constraintTpes)}"
    }

    private def generateVariableMap(args: List[Tree], body: Tree): VarMap = {
      val varmap = mutable.Map[Symbol, VarInfo]()
      var index = 0
      def addVar(s: Symbol) {
        varmap(s) = VarInfo(index)
        index += 1
      }
      for (t <- args) addVar(t.symbol)
      val traverser = new Traverser {
        override def traverse(t: Tree): Unit = t match {
          case q"$_ val $_: $_ = $rhs" =>
            addVar(t.symbol)
            traverse(rhs)
          case _ =>
            super.traverse(t)
        }
      }
      traverser.traverse(body)
      varmap
    }

    class CtrlNode(val tree: Tree) {
      var successors: List[CtrlNode] = Nil

      def prettyPrint = {
        val text = new StringBuilder
        var count = 0
        val seen = mutable.Map[CtrlNode, Int]()
        def print(n: CtrlNode, prefix: String) {
          def shorten(s: String) = {
            if (s.contains('\n')) s.takeWhile(_ != '\n') + "..." else s
          }
          seen(n) = count
          val treerepr = shorten(n.tree.toString)
          text.append(s"$prefix|-> $count: Node($treerepr)\n")
          count += 1
          def printChild(c: CtrlNode, newPrefix: String) {
            if (seen.contains(c)) {
              text.append(s"$newPrefix|-> label ${seen(c)}")
            } else {
              print(c, newPrefix)
            }
          }
          if (n.successors.nonEmpty) {
            for (s <- n.successors.tail) {
              printChild(s, prefix + "|   ")
            }
            printChild(n.successors.head, prefix)
          }
        }
        print(this, "")
        text.toString
      }
    }

    object CtrlNode {
      def withoutSuccessors(n: CtrlNode) = new CtrlNode(n.tree)
    }

    private def generateControlFlowGraph(body: Tree): CtrlNode = {
      def traverse(t: Tree): (CtrlNode, CtrlNode) = {
        t match {
          case q"if ($cond) $ifbranch else $elsebranch" =>
            val ifnode = new CtrlNode(t)
            val mergenode = new CtrlNode(q"{}")
            def addBranch(branch: Tree) {
              val (childhead, childlast) = traverse(branch)
              ifnode.successors ::= childhead
              childlast.successors ::= mergenode
            }
            addBranch(ifbranch)
            addBranch(elsebranch)
            (ifnode, mergenode)
          case q"{ ..$stats }" if stats.nonEmpty && stats.tail.nonEmpty =>
            val (first, childlast) = traverse(stats.head)
            var current = childlast
            for (stat <- stats.tail) {
              val (childhead, childlast) = traverse(stat)
              current.successors ::= childhead
              current = childlast
            }
            (first, current)
          case _ =>
            val n = new CtrlNode(t)
            (n, n)
        }
      }

      val (head, last) = traverse(body)
      println(head.prettyPrint)
      head
    }

    private def extractSubgraphs(cfg: CtrlNode): Set[CtrlNode] = {
      val subgraph = mutable.LinkedHashSet[CtrlNode]()
      val entrypoints = mutable.Set[CtrlNode]()
      val front = mutable.Queue[CtrlNode]()
      entrypoints += cfg
      front.enqueue(cfg)
      def extract(n: CtrlNode, seen: mutable.Map[CtrlNode, CtrlNode]): CtrlNode = {
        // duplicate and mark current node as seen
        val current = CtrlNode.withoutSuccessors(n)
        seen(n) = current

        // check for termination condition
        n.tree match {
          case q"coroutines.this.`package`.yieldval[$_]($_)" =>
            // add successors to node front
            for (s <- n.successors) if (!entrypoints(s)) {
              entrypoints += s
              front.enqueue(s)
            }
          case _ =>
            // traverse successors
            for (s <- n.successors) {
              if (!seen.contains(s)) {
                extract(s, seen)
              }
              current.successors ::= seen(s)
            }
        }
        current
      }

      // as long as there are more nodes on the expansion front, extract them
      while (front.nonEmpty) subgraph += extract(front.dequeue(), mutable.Map())
      println(subgraph.map(_.prettyPrint).zipWithIndex.map(t => s"\n${t._2}:\n${t._1}")
        .mkString("\n"))
      subgraph
    }

    private def generateEntryPoints(
      args: List[Tree], body: Tree, varmap: VarMap
    ): Map[Int, Tree] = {
      val cfg = generateControlFlowGraph(body)
      val segments = extractSubgraphs(cfg)

      Map(
        0 -> q"def ep0() = {}",
        1 -> q"def ep1() = {}"
      )
    }

    private def generateEnterMethod(entrypoints: Map[Int, Tree], tpe: Tree): Tree = {
      if (entrypoints.size == 1) {
        val q"def $ep() = $_" = entrypoints(0)

        q"""
        def enter(c: Coroutine[$tpe]): Unit = $ep()
        """
      } else if (entrypoints.size == 2) {
        val q"def $ep0() = $_" = entrypoints(0)
        val q"def $ep1() = $_" = entrypoints(1)

        q"""
        def enter(c: Coroutine[$tpe]): Unit = {
          val pc = scala.coroutines.common.Stack.top(c.pcstack)
          if (pc == 0) $ep0() else $ep1()
        }
        """
      } else {
        val cases = for ((index, defdef) <- entrypoints) yield {
          val q"def $ep() = $rhs" = defdef
          cq"$index => $ep()"
        }

        q"""
        def enter(c: Coroutine[$tpe]): Unit = {
          val pc = scala.coroutines.common.Stack.top(c.pcstack)
          (pc: @scala.annotation.switch) match {
            case ..$cases
          }
        }
        """
      }
    }

    def transform(f: Tree): Tree = {
      // ensure that argument is a function literal
      val (args, body) = f match {
        case q"(..$args) => $body" => (args, body)
        case _ => c.abort(f.pos, "The coroutine takes a single function literal.")
      }

      // extract argument names and types
      val (argnames, argtpes) = (for (arg <- args) yield {
        val q"$_ val $name: $tpe = $_" = arg
        (name, tpe)
      }).unzip

      // infer return type
      val rettpe = inferReturnType(body)

      // generate variable map
      val varmap = generateVariableMap(args, body)

      // generate entry points from yields and coroutine applies
      val entrypoints = generateEntryPoints(args, body, varmap)

      // generate entry method
      val entermethod = generateEnterMethod(entrypoints, rettpe)

      // emit coroutine instantiation
      val coroutineTpe = TypeName(s"Arity${args.size}")
      val entrypointmethods = entrypoints.map(_._2)
      val valnme = TermName(c.freshName("c"))
      val co = q"""new scala.coroutines.Coroutine.$coroutineTpe[..$argtpes, $rettpe] {
        def apply(..$args) = {
          val $valnme = new Coroutine[$rettpe]
          $valnme.push(this)
          $valnme
        }
        def push(c: Coroutine[$rettpe]): Unit = {
          ???
        }
        def pop(c: Coroutine[$rettpe]): Unit = {
          ???
        }
        $entermethod
        ..$entrypointmethods
      }"""
      println(co)
      co
    }
  }

  abstract class Arity0[@specialized T] extends Coroutine.Definition[T] {
    def apply(): Coroutine[T]
  }

  abstract class Arity1[A0, @specialized T] extends Coroutine.Definition[T] {
    def apply(a0: A0): Coroutine[T]
  }

  abstract class Arity2[A0, A1, @specialized T] extends Coroutine.Definition[T] {
    def apply(a0: A0, a1: A1): Coroutine[T]
  }

  abstract class Arity3[A0, A1, A2, @specialized T] extends Coroutine.Definition[T] {
    def apply(a0: A0, a1: A1, a2: A2): Coroutine[T]
  }
}
