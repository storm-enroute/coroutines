package scala.coroutines



import scala.annotation.tailrec
import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Synthesizes all coroutine-related functionality.
 */
private[coroutines] class Synthesizer[C <: Context](val c: C)
extends Analyzer[C] with ControlFlowGraph[C] {
  import c.universe._

  private def synthesizeEntryPoint(
    subgraph: SubCfg, rettpt: Tree
  )(implicit table: Table): Tree = {
    def findStart(chain: Chain): Zipper = {
      var z = {
        if (chain.parent == null) Zipper(null, Nil, trees => q"..$trees")
        else findStart(chain.parent).descend(trees => q"..$trees")
      }
      for ((sym, info) <- chain.vars) {
        if (subgraph.usesVar(sym) && !subgraph.declaresVar(sym)) {
          val cparam = table.names.coroutineParam
          val stack = info.stackname
          val pos = info.stackpos
          val stackget = q"scala.coroutines.common.Stack.get($cparam.$stack, $pos)"
          val decodedget = info.decodeLong(stackget)
          val valdef = info.origtree match {
            case q"$mods val $name: $tpt = $_" => q"$mods val $name: $tpt = $decodedget"
            case q"$mods var $name: $tpt = $_" => q"$mods var $name: $tpt = $decodedget"
          }
          z = z.append(valdef)
        }
      }
      z
    }

    val startPoint = findStart(subgraph.start.chain)
    val bodyZipper = subgraph.start.emitCode(startPoint, subgraph)
    val body = bodyZipper.root.result
    val defname = TermName(s"ep${subgraph.uid}")
    val defdef = q"""
      def $defname(${table.names.coroutineParam}: Coroutine[$rettpt]): Unit = {
        $body
      }
    """
    defdef
  }

  private def synthesizeEntryPoints(
    cfg: Cfg, rettpt: Tree
  )(implicit table: Table): Map[Long, Tree] = {
    val entrypoints = for ((orignode, subgraph) <- cfg.subgraphs) yield {
      (subgraph.uid, synthesizeEntryPoint(subgraph, rettpt))
    }
    entrypoints.toMap
  }

  private def synthesizeEnterMethod(
    entrypoints: Map[Long, Tree], tpt: Tree
  )(implicit table: Table): Tree = {
    if (entrypoints.size == 1) {
      val q"def $ep($_): Unit = $_" = entrypoints(0)

      q"""
        def enter(c: Coroutine[$tpt]): Unit = $ep(c)
      """
    } else if (entrypoints.size == 2) {
      val q"def $ep0($_): Unit = $_" = entrypoints(0)
      val q"def $ep1($_): Unit = $_" = entrypoints(1)

      q"""
        def enter(c: Coroutine[$tpt]): Unit = {
          val pc = scala.coroutines.common.Stack.top(c.pcstack)
          if (pc == 0) $ep0(c) else $ep1(c)
        }
      """
    } else {
      val cases = for ((index, defdef) <- entrypoints) yield {
        val q"def $ep($_): Unit = $rhs" = defdef
        cq"${index.toShort} => $ep(c)"
      }

      q"""
        def enter(c: Coroutine[$tpt]): Unit = {
          val pc: Short = scala.coroutines.common.Stack.top(c.pcstack)
          (pc: @scala.annotation.switch) match {
            case ..$cases
          }
        }
      """
    }
  }

  private def synthesizeReturnPoints(
    body: Tree, cfg: Cfg, rettpt: Tree
  )(implicit table: Table): Map[Long, Tree] = {
    // cfg.start.dfs.collect {
    //   case n @ ApplyCoroutine(tree, chain, uid) =>
    // }
    // val trees = body.collect {
    //   case t @ q"$_ val $_: $_ = $co.apply($_)" if isCoroutineDefType(co.tpe) =>
    //   case t @ q"$_ var $_: $_ = $co.apply($_)" if isCoroutineDefType(co.tpe) =>
    // }
    mutable.Map()
  }

  private def synthesizeReturnValueMethod(
    returnpoints: Map[Long, Tree], tpt: Tree
  )(implicit table: Table): Tree = {
    q"""
      def returnValue(c: scala.coroutines.Coroutine[$tpt], v: $tpt)(
        implicit cc: scala.coroutines.CanCallInternal
      ): Unit = {
        ???
      }
    """
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

    // generate control flow graph
    val cfg = generateControlFlowGraph(rettpt)

    // generate entry points from yields and coroutine applications
    val entrypoints = synthesizeEntryPoints(cfg, rettpt)

    // generate entry method
    val entermethod = synthesizeEnterMethod(entrypoints, rettpt)

    // generate return point methods for coroutine applications
    val returnpoints = synthesizeReturnPoints(body, cfg, rettpt)

    // generate return value method
    val returnvaluemethod = synthesizeReturnValueMethod(returnpoints, rettpt)

    // generate variable pushes and pops for stack variables
    val (varpushes, varpops) = (for ((sym, info) <- table.vars.toList) yield {
      (info.pushTree, info.popTree)
    }).unzip

    // emit coroutine instantiation
    val coroutineTpe = TypeName(s"Arity${args.size}")
    val entrypointmethods = entrypoints.map(_._2)
    val valnme = TermName(c.freshName("c"))
    val co = q"""
      new scala.coroutines.Coroutine.$coroutineTpe[..$argtpts, $rettpt] {
        def call(..$args)(
          implicit cc: scala.coroutines.CanCallInternal
        ): scala.coroutines.Coroutine[$rettpt] = {
          val $valnme = new scala.coroutines.Coroutine[$rettpt]
          push($valnme, ..$argidents)
          $valnme
        }
        def apply(..$args): $rettpt = {
          sys.error(
            "Coroutines can only be invoked directly from within other coroutines. " +
            "Use `call(<coroutine>(<arg0>, ..., <argN>))` instead if you want to " +
            "start a new coroutine.")
        }
        def push(c: scala.coroutines.Coroutine[$rettpt], ..$args)(
          implicit cc: scala.coroutines.CanCallInternal
        ): Unit = {
          scala.coroutines.common.Stack.push(c.costack, this, -1)
          scala.coroutines.common.Stack.push(c.pcstack, 0.toShort, -1)
          ..$varpushes
        }
        def pop(c: scala.coroutines.Coroutine[$rettpt]): Unit = {
          scala.coroutines.common.Stack.pop(c.pcstack)
          scala.coroutines.common.Stack.pop(c.costack)
          ..$varpops
        }
        $entermethod
        ..$entrypointmethods
        $returnvaluemethod
      }
    """
    println(co)
    co
  }

  def call[T: WeakTypeTag](lambda: Tree): Tree = {
    val (receiver, args) = lambda match {
      case q"$r.apply(..$args)" =>
        if (!isCoroutineDefType(r.tpe))
          c.abort(r.pos,
            s"Receiver must be a coroutine.\n" +
            s"required: Coroutine.Definition[${implicitly[WeakTypeTag[T]]}]\n" +
            s"found:    ${r.tpe} (with underlying type ${r.tpe.widen})")
        (r, args)
      case _ =>
        c.abort(
          lambda.pos,
          "The call statement must take a coroutine invocation expression:\n" +
          "  call(<coroutine>.apply(<arg0>, ..., <argN>))")
    }

    val tpe = implicitly[WeakTypeTag[T]]
    val t = q"""
      import scala.coroutines.Permission.canCall
      $receiver.call(..$args)
    """
    println(t)
    t
  }
}
