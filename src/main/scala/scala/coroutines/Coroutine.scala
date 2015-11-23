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
  private[coroutines] var rvstackptr = 0
  private[coroutines] var rvstack: Array[Byte] = _
  private[coroutines] var refstackptr = 0
  private[coroutines] var refstack: Array[AnyRef] = _
  private[coroutines] var valstackptr = 0
  private[coroutines] var valstack: Array[Long] = _
  private[coroutines] var target: Coroutine[T] = null
  private[coroutines] var result: T = null.asInstanceOf[T]

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
  }

  def transform(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).transform(f)
  }

  private[coroutines] class Synthesizer[C <: Context](val c: C) {
    import c.universe._

    // TODO: refactor this into a utility class
    val parserTrees = mutable.Map[Tree, Tree]()

    def getParserTree(t: Tree) = {
      if (parserTrees.contains(t)) parserTrees(t)
      else t
    }

    // TODO: refactor this into a utility class
    def traverseMirrored(t0: Tree, t1: Tree)(f: (Tree, Tree) => Unit) = {
      def traverse(t0: Tree, t1: Tree): Unit = {
        f(t0, t1)
        (t0, t1) match {
          case (q"(..$args0) => $body0", q"(..$args1) => $body1") =>
            for ((a0, a1) <- args0 zip args1) traverse(a0, a1)
            traverse(body0, body1)
          case (q"$_ val $_: $tp0 = $rhs0", q"$_ val $_: $tp1 = $rhs1") =>
            traverse(tp0, tp1)
            traverse(rhs0, rhs1)
          case (q"if ($c0) $t0 else $e0", q"if ($c1) $t1 else $e1") =>
            traverse(c0, c1)
            traverse(t0, t1)
            traverse(e0, e1)
          case (q"$r0.$m0(..$args0)", q"$r1.$m1(..$args1)") =>
            traverse(r0, r1)
            for ((a0, a1) <- args0 zip args1) traverse(a0, a1)
          case (q"$r0.$m0", q"$r1.$m1") =>
            traverse(r0, r1)
          case (q"{ ..$ss0 }", q"{ ..$ss1 }") if ss0.length > 1 && ss1.length > 1 =>
            for ((a, b) <- ss0 zip ss1) traverse(a, b)
          case _ =>
            // TODO: implement remaining cases
        }
      }
      traverse(t0, t1)
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

    class VarMap(val lambda: Tree) {
      var varcount = 0
      val all = mutable.LinkedHashMap[Symbol, VarInfo]()
      val topChain = new Chain(this, lambda, null)
      def foreach[U](f: ((Symbol, VarInfo)) => U): Unit = all.foreach(f)
      def contains(s: Symbol) = all.contains(s)
      def apply(s: Symbol) = all(s)
      def refvars = all.filter(_._2.isRefType)
      def valvars = all.filter(_._2.isValType)
    }

    class Chain(val varmap: VarMap, val origtree: Tree, val parent: Chain) {
      val vars = mutable.LinkedHashMap[Symbol, VarInfo]()
      def newChain(subtree: Tree) = new Chain(varmap, subtree, this)
      def addVar(valdef: Tree, name: TermName, isArg: Boolean) {
        val sym = valdef.symbol
        val info = new VarInfo(varmap.varcount, valdef, sym.info, sym, name, isArg)
        vars(sym) = info
        varmap.all(sym) = info
        varmap.varcount += 1
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

    class Subgraph {
      val referencedvars = mutable.LinkedHashMap[Symbol, VarInfo]()
      val declaredvars = mutable.LinkedHashMap[Symbol, VarInfo]()
      var start: CtrlNode = _
    }

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

    private def inferReturnType(body: Tree): Tree = {
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

    private def generateControlFlowGraph(
      args: List[Tree], body: Tree, varmap: VarMap
    ): CtrlNode = {
      def traverse(t: Tree, c: Chain): (CtrlNode, CtrlNode) = {
        t match {
          case q"$_ val $name: $_ = $_" =>
            c.addVar(t, name, false)
            val n = new CtrlNode(t, None, c)
            (n, n)
          case q"if ($cond) $thenbranch else $elsebranch" =>
            val ifnode = new CtrlNode(t, Some(t), c)
            val mergenode = new CtrlNode(q"{}", Some(t), c)
            def addBranch(branch: Tree) {
              val nestedchain = c.newChain(t)
              val (childhead, childlast) = traverse(branch, nestedchain)
              ifnode.successors ::= childhead
              childlast.successors ::= mergenode
            }
            addBranch(thenbranch)
            addBranch(elsebranch)
            (ifnode, mergenode)
          case q"{ ..$stats }" if stats.nonEmpty && stats.tail.nonEmpty =>
            val nestedchain = c.newChain(t)
            val (first, childlast) = traverse(stats.head, nestedchain)
            var current = childlast
            for (stat <- stats.tail) {
              val (childhead, childlast) = traverse(stat, nestedchain)
              current.successors ::= childhead
              current = childlast
            }
            (first, current)
          case _ =>
            val n = new CtrlNode(t, None, c)
            (n, n)
        }
      }

      for (t <- args) {
        val q"$_ val $name: $_ = $_" = t
        varmap.topChain.addVar(t, name, true)
      }

      // traverse tree to construct CFG and extract local variables
      val (head, last) = traverse(body, varmap.topChain)
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
        val current = CtrlNode.copyNoSuccessors(n)
        seen(n) = current

        // detect referenced and declared stack variables
        for (t <- n.tree) {
          if (varmap.contains(t.symbol)) {
            subgraph.referencedvars(t.symbol) = varmap(t.symbol)
          }
          t match {
            case q"$_ val $_: $_ = $_" =>
              subgraph.declaredvars(t.symbol) = varmap(t.symbol)
            case _ =>
              // do nothing
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
        .map(t => {
          "[" + t.referencedvars.keys.mkString(", ") + "]\n" + t.start.prettyPrint
        })
        .zipWithIndex.map(t => s"\n${t._2}:\n${t._1}")
        .mkString("\n"))
      subgraphs
    }

    private def generateEntryPoint(varmap: VarMap, i: Int, subgraph: Subgraph): Tree = {
      def findStart(chain: Chain): Zipper = {
        var z = {
          if (chain.parent == null) Zipper(null, Nil, trees => q"..$trees")
          else findStart(chain.parent).descend(trees => q"..$trees")
        }
        for ((sym, info) <- chain.vars) {
          val referenced = subgraph.referencedvars.contains(sym)
          val declared = subgraph.declaredvars.contains(sym)
          if (referenced && !declared) {
            val q"$mods val $name: $tpt = $_" = info.origtree
            val valdef = q"$mods val $name: $tpt = ${info.defaultValue}"
            z = z.append(valdef)
          }
        }
        z
      }

      def construct(
        n: CtrlNode, seen: mutable.Set[CtrlNode], z: Zipper
      ): Zipper = {
        if (!seen(n)) {
          seen += n
          n.ctrlflowtree match {
            case None =>
              // inside the control-flow-construct, normal statement
              val z1 = z.append(getParserTree(n.tree))
              n.singleSuccessor match {
                case Some(sn) => construct(sn, seen, z1)
                case None => z1
              }
            case Some(cftree) if cftree eq n.tree =>
              // node marks the start of a control-flow-construct
              cftree match {
                case q"if ($cond) $_ else $_" =>
                  val newZipper = Zipper(null, Nil, trees => q"..$trees")
                  val elsenode = n.successors(0)
                  val thennode = n.successors(1)
                  val elsebranch = construct(elsenode, seen, newZipper).root.result
                  val thenbranch = construct(thennode, seen, newZipper).root.result
                  val parsercond = getParserTree(cond)
                  val iftree = q"if ($parsercond) $thenbranch else $elsebranch"
                  val z1 = z.append(iftree)
                  z1
                case _ =>
                  sys.error("Unknown control flow construct: $cftree")
              }
            case Some(cftree) if cftree ne n.tree =>
              // node marks the end of a control-flow-construct
              val z1 = construct(n.backwardSuccessor, seen, z)
              val z2 = construct(n.forwardSuccessor, seen, z1)
              z2
          }
        } else z
      }

      val startPoint = findStart(subgraph.start.chain)
      val bodyZipper = construct(subgraph.start, mutable.Set(), startPoint)
      val body = bodyZipper.root.result
      val defname = TermName(s"ep$i")
      val defdef = q"""
        def $defname(): Unit = {
          $body
        }
      """
      defdef
    }

    private def generateEntryPoints(
      args: List[Tree], body: Tree, varmap: VarMap, rettpt: Tree
    ): Map[Int, Tree] = {
      val cfg = generateControlFlowGraph(args, body, varmap)
      val subgraphs = extractSubgraphs(varmap, cfg, rettpt)

      val entrypoints = for ((subgraph, i) <- subgraphs.zipWithIndex) yield {
        (i, generateEntryPoint(varmap, i, subgraph))
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
      val varmap = new VarMap(f)
      val parserf = c.untypecheck(f)
      traverseMirrored(f, parserf)((t, pt) => parserTrees(t) = pt)

      // ensure that argument is a function literal
      val (args, body) = f match {
        case q"(..$args) => $body" => (args, body)
        case _ => c.abort(f.pos, "The coroutine takes a single function literal.")
      }
      val argidents = for (arg <- args) yield {
        val q"$_ val $argname: $_ = $_" = arg
        q"$argname"
      }

      // extract argument names and types
      val (argnames, argtpts) = (for (arg <- args) yield {
        val q"$_ val $name: $tpt = $_" = arg
        (name, tpt)
      }).unzip

      // infer coroutine return type
      val rettpt = inferReturnType(body)

      // generate entry points from yields and coroutine applies
      val entrypoints = generateEntryPoints(args, body, varmap, rettpt)

      // generate entry method
      val entermethod = generateEnterMethod(entrypoints, rettpt)

      // generate variable pushes and pops for stack variables
      val (varpushes, varpops) = (for ((sym, info) <- varmap.all.toList) yield {
        (info.pushTree, info.popTree)
      }).unzip

      // emit coroutine instantiation
      val coroutineTpe = TypeName(s"Arity${args.size}")
      val entrypointmethods = entrypoints.map(_._2)
      val valnme = TermName(c.freshName("c"))
      val co = q"""
        new scala.coroutines.Coroutine.$coroutineTpe[..$argtpts, $rettpt] {
          def call(..$args) = {
            val $valnme = new Coroutine[$rettpt]
            push($valnme, ..$argidents)
            $valnme
          }
          def apply(..$args): $rettpt = {
            sys.error(
              "Coroutines can only be invoked directly from within other coroutines. " +
              "Use `call` instead if you want to start a new coroutine.")
          }
          def push(c: Coroutine[$rettpt], ..$args): Unit = {
            scala.coroutines.common.Stack.push(c.costack, this, -1)
            scala.coroutines.common.Stack.push(c.pcstack, 0.toShort, -1)
            ..$varpushes
          }
          def pop(c: Coroutine[$rettpt]): Unit = {
            scala.coroutines.common.Stack.pop(c.pcstack)
            scala.coroutines.common.Stack.pop(c.costack)
            ..$varpops
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
