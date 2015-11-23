package scala.coroutines



import scala.annotation.tailrec
import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



private[coroutines] class Synthesizer[C <: Context](val c: C)
extends Analyzer[C] with ControlFlowGraph[C] {
  import c.universe._

  class Subgraph {
    val referencedvars = mutable.LinkedHashMap[Symbol, VarInfo]()
    val declaredvars = mutable.LinkedHashMap[Symbol, VarInfo]()
    var start: Node = _
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

  private def extractSubgraphs(
    cfg: Node, rettpt: Tree
  )(implicit table: Table): Set[Subgraph] = {
    val subgraphs = mutable.LinkedHashSet[Subgraph]()
    val seenEntries = mutable.Set[Node]()
    val nodefront = mutable.Queue[Node]()
    seenEntries += cfg
    nodefront.enqueue(cfg)

    def extract(
      n: Node, seen: mutable.Map[Node, Node], subgraph: Subgraph
    ): Node = {
      // duplicate and mark current node as seen
      val current = n.copyWithoutSuccessors
      seen(n) = current

      // detect referenced and declared stack variables
      for (t <- n.tree) {
        if (table.contains(t.symbol)) {
          subgraph.referencedvars(t.symbol) = table(t.symbol)
        }
        t match {
          case q"$_ val $_: $_ = $_" =>
            subgraph.declaredvars(t.symbol) = table(t.symbol)
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

  private def synthesizeEntryPoint(
    i: Int, subgraph: Subgraph, rettpt: Tree
  )(implicit table: Table): Tree = {
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

    val startPoint = findStart(subgraph.start.chain)
    val bodyZipper = subgraph.start.emitCode(startPoint)
    val body = bodyZipper.root.result
    val defname = TermName(s"ep$i")
    val defdef = q"""
      def $defname(${table.names.coroutineParam}: Coroutine[$rettpt]): Unit = {
        $body
      }
    """
    defdef
  }

  private def synthesizeEntryPoints(
    args: List[Tree], body: Tree, rettpt: Tree
  )(implicit table: Table): Map[Int, Tree] = {
    val cfg = generateControlFlowGraph()
    val subgraphs = extractSubgraphs(cfg, rettpt)

    val entrypoints = for ((subgraph, i) <- subgraphs.zipWithIndex) yield {
      (i, synthesizeEntryPoint(i, subgraph, rettpt))
    }
    entrypoints.toMap
  }

  private def synthesizeEnterMethod(
    entrypoints: Map[Int, Tree], tpt: Tree
  )(implicit table: Table): Tree = {
    val cparamname = table.names.coroutineParam
    if (entrypoints.size == 1) {
      val q"def $ep($_): Unit = $_" = entrypoints(0)

      q"""
        def enter($cparamname: Coroutine[$tpt]): Unit = $ep($cparamname)
      """
    } else if (entrypoints.size == 2) {
      val q"def $ep0($_): Unit = $_" = entrypoints(0)
      val q"def $ep1($_): Unit = $_" = entrypoints(1)

      q"""
        def enter($cparamname: Coroutine[$tpt]): Unit = {
          val pc = scala.coroutines.common.Stack.top($cparamname.pcstack)
          if (pc == 0) $ep0($cparamname) else $ep1($cparamname)
        }
      """
    } else {
      val cases = for ((index, defdef) <- entrypoints) yield {
        val q"def $ep($_): Unit = $rhs" = defdef
        cq"$index => $ep($cparamname)"
      }

      q"""
        def enter($cparamname: Coroutine[$tpt]): Unit = {
          val pc = scala.coroutines.common.Stack.top($cparamname.pcstack)
          (pc: @scala.annotation.switch) match {
            case ..$cases
          }
        }
      """
    }
  }

  def synthesize(lambda: Tree): Tree = {
    implicit val table = new Table(lambda)

    // ensure that argument is a function literal
    val (args, body) = lambda match {
      case q"(..$args) => $body" => (args, body)
      case _ => c.abort(lambda.pos, "The coroutine takes a single function literal.")
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
    val entrypoints = synthesizeEntryPoints(args, body, rettpt)

    // generate entry method
    val entermethod = synthesizeEnterMethod(entrypoints, rettpt)

    // generate variable pushes and pops for stack variables
    val (varpushes, varpops) = (for ((sym, info) <- table.all.toList) yield {
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
