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
  private[coroutines] var refstackptr = 0
  private[coroutines] var refstack: Array[AnyRef] = _
  private[coroutines] var valstackptr = 0
  private[coroutines] var valstack: Array[Long] = _
  private[coroutines] var target: Coroutine[T] = null
  private[coroutines] var result: T = null.asInstanceOf[T]

  final def startFrame(cd: Coroutine.Definition[T]) {
    Stack.push(costack, cd)
    Stack.push(pcstack, 0.toShort)
    cd.push(this)
  }

  final def endFrame() {
    val cd = Stack.top(costack)
    cd.pop(this)
    Stack.pop(pcstack)
    Stack.pop(costack)
  }

  def apply(): T = Coroutine.enter[T](this)
}


object Coroutine {
  private[coroutines] val INITIAL_CO_STACK_SIZE = 4

  @tailrec
  private[coroutines] final def enter[T](c: Coroutine[T]): T = {
    val cd = Stack.top(c.costack)
    cd.enter(c)
    if (c.target ne null) {
      val nc = c.target
      c.target = null
      enter(nc)
    } else c.result
  }

  abstract class Definition[T] {
    def enter(c: Coroutine[T]): Unit
    def push(c: Coroutine[T]): Unit
    def pop(c: Coroutine[T]): Unit
  }

  def transform(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).transform(f)
  }

  private[coroutines] class Synthesizer[C <: Context](val c: C) {
    import c.universe._

    class VarInfo(val uid: Int, val tpe: Type) {
      var stackpos = uid
    }

    type VarMap = mutable.Map[Symbol, VarInfo]

    implicit class VarMapOps(val self: VarMap) {
      def refvars = self.filter(_._2.tpe <:< typeOf[AnyRef])
      def valvars = self.filter(_._2.tpe <:< typeOf[AnyVal])
    }

    def newVarMap: VarMap = mutable.Map[Symbol, VarInfo]()

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

    class Subgraph {
      val stackvars = newVarMap
      var start: CtrlNode = _
    }

    private def inferReturnType(body: Tree): Tree = {
      // return type must correspond to the return type of the function literal
      val rettpe = body.tpe

      // return type is the lub of the function return type and yield argument types
      def isCoroutinesPackage(q: Tree) = q match {
        case q"coroutines.this.`package`" => true
        case t => false
      }
      // TODO: ensure that this does not capture constraints from nested class scopes
      val constraintTpes = body.collect {
        case q"$qual.yieldval[$tpt]($v)" if isCoroutinesPackage(qual) => tpt.tpe
        case q"$qual.yieldto[$tpt]($f)" if isCoroutinesPackage(qual) => tpt.tpe
      }
      tq"${lub(rettpe :: constraintTpes)}"
    }

    private def generateVariableMap(args: List[Tree], body: Tree): VarMap = {
      val varmap = newVarMap
      var index = 0
      def addVar(s: Symbol) {
        varmap(s) = new VarInfo(index, s.info)
        index += 1
      }
      for (t <- args) addVar(t.symbol)
      // TODO: ensure that this does not capture variables from nested class scopes
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

    private def extractSubgraphs(
      varmap: VarMap, cfg: CtrlNode, rettpt: Tree
    ): Set[Subgraph] = {
      val subgraphs = mutable.LinkedHashSet[Subgraph]()
      val seenEntries = mutable.Set[CtrlNode]()
      val nodefront = mutable.Queue[CtrlNode]()
      seenEntries += cfg
      nodefront.enqueue(cfg)

      def extract(
        n: CtrlNode, seen: mutable.Map[CtrlNode, CtrlNode], subgraph: Subgraph
      ): CtrlNode = {
        // duplicate and mark current node as seen
        val current = CtrlNode.withoutSuccessors(n)
        seen(n) = current

        // detect referenced stack variables
        for (t <- n.tree) if (varmap.contains(t.symbol)) {
          subgraph.stackvars(t.symbol) = varmap(t.symbol)
        }

        // check for termination condition
        def addToNodeFront() {
          // add successors to node front
          for (s <- n.successors) if (!seenEntries(s)) {
            seenEntries += s
            nodefront.enqueue(s)
          }
        }
        def isCoroutineDefType(tpe: Type) = {
          val codefsym = typeOf[Coroutine.Definition[_]].typeConstructor.typeSymbol
          tpe.baseType(codefsym) != NoType
        }
        def addCoroutineInvocationToNodeFront(co: Tree) {
          val codeftpe = typeOf[Coroutine.Definition[_]].typeConstructor
          val coroutinetpe = appliedType(codeftpe, List(rettpt.tpe))
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
          case q"coroutines.this.`package`.yieldto[$_]($_)" =>
            addToNodeFront()
          case q"return $_" =>
            addToNodeFront()
          case q"$_ val $_ = $co.apply(..$args)" if isCoroutineDefType(co.tpe) =>
            addCoroutineInvocationToNodeFront(co)
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
        val subgraph = new Subgraph
        subgraph.start = extract(nodefront.dequeue(), mutable.Map(), subgraph)
        subgraphs += subgraph
      }
      println(subgraphs
        .map(t => "[" + t.stackvars.mkString(", ") + "]\n" + t.start.prettyPrint)
        .zipWithIndex.map(t => s"\n${t._2}:\n${t._1}")
        .mkString("\n"))
      subgraphs
    }

    private def generateEntryPoints(
      args: List[Tree], body: Tree, varmap: VarMap, rettpt: Tree
    ): Map[Int, Tree] = {
      val cfg = generateControlFlowGraph(body)
      val subgraphs = extractSubgraphs(varmap, cfg, rettpt)

      val entrypoints = for ((subgraph, i) <- subgraphs.zipWithIndex) yield {
        val defname = TermName(s"ep$i")
        val defdef = q"""
          def $defname(): Unit = {
            ???
          }
        """
        (i, defdef)
      }
      entrypoints.toMap
    }

    private def generateEnterMethod(entrypoints: Map[Int, Tree], tpt: Tree): Tree = {
      if (entrypoints.size == 1) {
        val q"def $ep(): Unit = $_" = entrypoints(0)

        q"""
        def enter(c: Coroutine[$tpt]): Unit = $ep()
        """
      } else if (entrypoints.size == 2) {
        val q"def $ep0(): Unit = $_" = entrypoints(0)
        val q"def $ep1(): Unit = $_" = entrypoints(1)

        q"""
        def enter(c: Coroutine[$tpt]): Unit = {
          val pc = scala.coroutines.common.Stack.top(c.pcstack)
          if (pc == 0) $ep0() else $ep1()
        }
        """
      } else {
        val cases = for ((index, defdef) <- entrypoints) yield {
          val q"def $ep(): Unit = $rhs" = defdef
          cq"$index => $ep()"
        }

        q"""
        def enter(c: Coroutine[$tpt]): Unit = {
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
      val (argnames, argtpts) = (for (arg <- args) yield {
        val q"$_ val $name: $tpt = $_" = arg
        (name, tpt)
      }).unzip

      // infer return type
      val rettpt = inferReturnType(body)

      // generate variable map
      val varmap = generateVariableMap(args, body)

      // generate entry points from yields and coroutine applies
      val entrypoints = generateEntryPoints(args, body, varmap, rettpt)

      // generate entry method
      val entermethod = generateEnterMethod(entrypoints, rettpt)

      // emit coroutine instantiation
      val coroutineTpe = TypeName(s"Arity${args.size}")
      val entrypointmethods = entrypoints.map(_._2)
      val valnme = TermName(c.freshName("c"))
      val co = q"""
        new scala.coroutines.Coroutine.$coroutineTpe[..$argtpts, $rettpt] {
          def call(..$args) = {
            val $valnme = new Coroutine[$rettpt]
            $valnme.startFrame(this)
            $valnme
          }
          def apply(..$args): $rettpt = {
            sys.error(
              "Coroutines can only be invoked directly from within other coroutines. " +
              "Use `call` instead if you want to start a new coroutine.")
          }
          def push(c: Coroutine[$rettpt]): Unit = {

          }
          def pop(c: Coroutine[$rettpt]): Unit = {
            
          }
          $entermethod
          ..$entrypointmethods
        }
      """
      println(co)
      co
    }
  }

  abstract class Arity0[@specialized T] extends Coroutine.Definition[T] {
    def call(): Coroutine[T]
    def apply(): T
  }

  abstract class Arity1[A0, @specialized T] extends Coroutine.Definition[T] {
    def call(a0: A0): Coroutine[T]
    def apply(a0: A0): T
  }

  abstract class Arity2[A0, A1, @specialized T] extends Coroutine.Definition[T] {
    def call(a0: A0, a1: A1): Coroutine[T]
    def apply(a0: A0, a1: A1): T
  }

  abstract class Arity3[A0, A1, A2, @specialized T] extends Coroutine.Definition[T] {
    def call(a0: A0, a1: A1, a2: A2): Coroutine[T]
    def apply(a0: A0, a1: A1, a2: A2): T
  }
}
