package scala.coroutines



import scala.annotation.tailrec
import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Synthesizes all coroutine-related functionality.
 */
private[coroutines] class Synthesizer[C <: Context](val c: C)
extends Analyzer[C]
with CfgGenerator[C]
with ThreeAddressFormTransformation[C] {
  import c.universe._

  val NUM_PREDEFINED_ENTRY_STUBS = 40

  private def genEntryPoint(cfg: Cfg, subgraph: SubCfg)(
    implicit t: Table
  ): Tree = {
    val body = subgraph.emit(cfg)
    val defname = TermName(s"$$ep${subgraph.uid}")
    val defdef = if (subgraph.uid < NUM_PREDEFINED_ENTRY_STUBS) q"""
      override def $defname(
        ${t.names.coroutineParam}: Coroutine.Inst[${t.yieldType}, ${t.returnType}]
      ): Unit = {
        $body
      }
    """ else q"""
      def $defname(
        ${t.names.coroutineParam}: Coroutine.Inst[${t.yieldType}, ${t.returnType}]
      ): Unit = {
        $body
      }
    """
    defdef
  }

  private def genEntryPoints(cfg: Cfg)(implicit table: Table): Map[Long, Tree] = {
    val entrypoints = for ((orignode, subgraph) <- cfg.subgraphs) yield {
      (subgraph.uid, genEntryPoint(cfg, subgraph))
    }
    mutable.LinkedHashMap() ++= entrypoints.toSeq.sortBy(_._1)
  }

  private def genEnterMethod(entrypoints: Map[Long, Tree])(
    implicit table: Table
  ): Tree = {
    val rettpt = table.returnType
    val yldtpt = table.yieldType
    if (entrypoints.size == 1) {
      val q"$_ def $ep0($_): Unit = $_" = entrypoints(0)

      q"""
        def $$enter(c: Coroutine.Inst[$yldtpt, $rettpt]): Unit = $ep0(c)
      """
    } else if (entrypoints.size == 2) {
      val q"$_ def $ep0($_): Unit = $_" = entrypoints(0)
      val q"$_ def $ep1($_): Unit = $_" = entrypoints(1)

      q"""
        def $$enter(c: Coroutine.Inst[$yldtpt, $rettpt]): Unit = {
          val pc = scala.coroutines.common.Stack.top(c.$$pcstack)
          if (pc == 0) $ep0(c) else $ep1(c)
        }
      """
    } else {
      val cases = for ((index, defdef) <- entrypoints) yield {
        val q"$_ def $ep($_): Unit = $rhs" = defdef
        cq"${index.toShort} => $ep(c)"
      }

      q"""
        def $$enter(c: Coroutine.Inst[$yldtpt, $rettpt]): Unit = {
          val pc: Short = scala.coroutines.common.Stack.top(c.$$pcstack)
          (pc: @scala.annotation.switch) match {
            case ..$cases
          }
        }
      """
    }
  }

  private def genReturnValueMethod(cfg: Cfg)(implicit table: Table): Tree = {
    def genReturnValueStore(n: Node) = {
      val sub = cfg.subgraphs(n.successors.head)
      val pcvalue = sub.uid
      val info = table(n.tree.symbol)
      val rvset = info.storeTree(q"c", q"v")
      (pcvalue, q"$rvset")
    }
    val returnstores = cfg.start.dfs.collect {
      case n @ Node.ApplyCoroutine(_, _, _) => genReturnValueStore(n)
    }

    val body = {
      if (returnstores.size == 0) {
        q"()"
      } else if (returnstores.size == 1) {
        returnstores(0)._2
      } else if (returnstores.size == 2) {
        q"""
          val pc = scala.coroutines.common.Stack.top(c.$$pcstack)
          if (pc == ${returnstores(0)._1.toShort}) {
            ${returnstores(0)._2}
          } else {
            ${returnstores(1)._2}
          }
        """
      } else {
        val cases = for ((pcvalue, rvset) <- returnstores) yield {
          cq"${pcvalue.toShort} => $rvset"
        }
        q"""
          val pc = scala.coroutines.common.Stack.top(c.$$pcstack)
          (pc: @scala.annotation.switch) match {
            case ..$cases
          }
        """
      }
    }

    q"""
      def $$returnvalue(
        c: scala.coroutines.Coroutine.Inst[${table.yieldType}, ${table.returnType}],
        v: ${table.returnType}
      ): Unit = {
        $body
      }
    """
  }

  def genVarPushesAndPops(cfg: Cfg)(implicit table: Table): (List[Tree], List[Tree]) = {
    val stackVars = cfg.stackVars
    val storedValVars = cfg.storedValVars
    val storedRefVars = cfg.storedRefVars
    def stackSize(vs: Map[Symbol, VarInfo]) = vs.map(_._2.stackpos._2).sum
    def genVarPushes(allvars: Map[Symbol, VarInfo], stack: Tree): List[Tree] = {
      val vars = allvars.filter(kv => stackVars.contains(kv._1))
      val varsize = stackSize(vars)
      val stacksize = math.max(table.initialStackSize, varsize)
      val bulkpushes = if (vars.size == 0) Nil else List(q"""
        scala.coroutines.common.Stack.bulkPush($stack, $varsize, $stacksize)
      """)
      val args = vars.values.filter(_.isArg).toList
      val argstores = for (a <- args) yield a.storeTree(q"c", q"${a.name}")
      bulkpushes ::: argstores
    }
    val varpushes = {
      genVarPushes(storedRefVars, q"c.$$refstack") ++
      genVarPushes(storedValVars, q"c.$$valstack")
    }
    val varpops = (for ((sym, info) <- storedRefVars.toList) yield {
      info.popTree
    }) ++ (if (storedValVars.size == 0) Nil else List(
      q"""
        scala.coroutines.common.Stack.bulkPop(c.$$valstack, ${stackSize(storedValVars)})
      """
    ))
    (varpushes, varpops)
  }

  def specArity1(
    argtpts: List[Tree], yldtpt: Tree, rettpt: Tree
  ): (Tree, List[Tree]) = {
    argtpts(0) match {
      case tq"scala.Boolean" =>
        (tq"scala.coroutines.Coroutine._1", argtpts :+ yldtpt :+ rettpt)
      case tq"scala.Byte" =>
        (tq"scala.coroutines.Coroutine._1", argtpts :+ yldtpt :+ rettpt)
      case tq"scala.Short" =>
        val nme = TypeName(s"_1$$spec$$S")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case tq"scala.Char" =>
        val nme = TypeName(s"_1$$spec$$C")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case tq"scala.Int" =>
        val nme = TypeName(s"_1$$spec$$I")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case tq"scala.Float" =>
        val nme = TypeName(s"_1$$spec$$F")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case tq"scala.Long" =>
        val nme = TypeName(s"_1$$spec$$J")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case tq"scala.Double" =>
        val nme = TypeName(s"_1$$spec$$D")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case _ =>
        val nme = TypeName(s"_1$$spec$$L")
        (tq"scala.coroutines.$nme", argtpts :+ yldtpt :+ rettpt)
    }
  }

  def specArity2(
    argtpts: List[Tree], yldtpt: Tree, rettpt: Tree
  ): (Tree, List[Tree]) = {
    (argtpts(0), argtpts(1)) match {
      case (tq"scala.Int", tq"scala.Int") =>
        val nme = TypeName(s"_2$$spec$$II")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (tq"scala.Long", tq"scala.Int") =>
        val nme = TypeName(s"_2$$spec$$JI")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (tq"scala.Double", tq"scala.Int") =>
        val nme = TypeName(s"_2$$spec$$DI")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (_, tq"scala.Int") =>
        val nme = TypeName(s"_2$$spec$$LI")
        (tq"scala.coroutines.$nme", argtpts(0) :: yldtpt :: rettpt :: Nil)
      case (tq"scala.Int", tq"scala.Long") =>
        val nme = TypeName(s"_2$$spec$$IJ")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (tq"scala.Long", tq"scala.Long") =>
        val nme = TypeName(s"_2$$spec$$JJ")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (tq"scala.Double", tq"scala.Long") =>
        val nme = TypeName(s"_2$$spec$$DJ")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (_, tq"scala.Long") =>
        val nme = TypeName(s"_2$$spec$$LJ")
        (tq"scala.coroutines.$nme", argtpts(0) :: yldtpt :: rettpt :: Nil)
      case (tq"scala.Int", tq"scala.Double") =>
        val nme = TypeName(s"_2$$spec$$ID")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (tq"scala.Long", tq"scala.Double") =>
        val nme = TypeName(s"_2$$spec$$JD")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (tq"scala.Double", tq"scala.Double") =>
        val nme = TypeName(s"_2$$spec$$DD")
        (tq"scala.coroutines.$nme", yldtpt :: rettpt :: Nil)
      case (_, tq"scala.Double") =>
        val nme = TypeName(s"_2$$spec$$LD")
        (tq"scala.coroutines.$nme", argtpts(0) :: yldtpt :: rettpt :: Nil)
      case (tq"scala.Int", _) =>
        val nme = TypeName(s"_2$$spec$$IL")
        (tq"scala.coroutines.$nme", argtpts(1) :: yldtpt :: rettpt :: Nil)
      case (tq"scala.Long", _) =>
        val nme = TypeName(s"_2$$spec$$JL")
        (tq"scala.coroutines.$nme", argtpts(1) :: yldtpt :: rettpt :: Nil)
      case (tq"scala.Double", _) =>
        val nme = TypeName(s"_2$$spec$$DL")
        (tq"scala.coroutines.$nme", argtpts(1) :: yldtpt :: rettpt :: Nil)
      case (_, _) =>
        val nme = TypeName(s"_2$$spec$$LL")
        (tq"scala.coroutines.$nme", argtpts(0) :: argtpts(1) :: yldtpt :: rettpt :: Nil)
    }
  }

  def genCoroutineTpe(
    argtpts: List[Tree], yldtpt: Tree, rettpt: Tree
  ): (Tree, List[Tree]) = {
    if (argtpts.length == 1) {
      specArity1(argtpts, yldtpt, rettpt)
    } else if (argtpts.length == 2) {
      specArity2(argtpts, yldtpt, rettpt)
    } else if (argtpts.length == 0 || argtpts.length > 2) {
      val nme = TypeName(s"_${argtpts.size}")
      (tq"scala.coroutines.Coroutine.$nme", argtpts :+ yldtpt :+ rettpt)
    } else sys.error("Unreachable case.")
  }

  def synthesize(rawlambda: Tree): Tree = {
    // transform to two operand assignment form
    val typedtaflambda = transformToThreeAddressForm(rawlambda)
    // println(typedtaflambda)
    // println(typedtaflambda.tpe)

    implicit val table = new Table(typedtaflambda)
    
    // ensure that argument is a function literal
    val q"(..$args) => $body" = typedtaflambda
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
    val rettpt = table.returnType
    val yldtpt = table.yieldType

    // generate control flow graph
    val cfg = genControlFlowGraph(args, body, rettpt)

    // generate entry points from yields and coroutine applications
    val entrypoints = genEntryPoints(cfg)

    // generate entry method
    val entermethod = genEnterMethod(entrypoints)

    // generate return value method
    val returnvaluemethod = genReturnValueMethod(cfg)

    // generate variable pushes and pops for stack variables
    val (varpushes, varpops) = genVarPushesAndPops(cfg)

    // emit coroutine instantiation
    val (coroutinequal, tparams) = genCoroutineTpe(argtpts, yldtpt, rettpt)
    val entrypointmethods = entrypoints.map(_._2)
    val valnme = TermName(c.freshName("c"))
    val co = q"""
      new $coroutinequal[..$tparams] {
        def $$call(..$args): scala.coroutines.Coroutine.Inst[$yldtpt, $rettpt] = {
          val $valnme = new scala.coroutines.Coroutine.Inst[$yldtpt, $rettpt]
          $$push($valnme, ..$argidents)
          $valnme
        }
        def apply(..$args): $rettpt = {
          sys.error(scala.coroutines.COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
        }
        def $$push(
          c: scala.coroutines.Coroutine.Inst[$yldtpt, $rettpt], ..$args
        ): Unit = {
          scala.coroutines.common.Stack.push(c.$$costack, this, -1)
          scala.coroutines.common.Stack.push(c.$$pcstack, 0.toShort, -1)
          ..$varpushes
        }
        def $$pop(c: scala.coroutines.Coroutine.Inst[$yldtpt, $rettpt]): Unit = {
          scala.coroutines.common.Stack.pop(c.$$pcstack)
          scala.coroutines.common.Stack.pop(c.$$costack)
          ..$varpops
        }
        $entermethod
        ..$entrypointmethods
        $returnvaluemethod
      }
    """
    //println(co)
    co
  }

  def call[R: WeakTypeTag](tree: Tree): Tree = {
    val (receiver, args) = tree match {
      case q"$r.apply(..$args)" =>
        // if (!isCoroutineDefMarker(r.tpe))
        //   c.abort(r.pos,
        //     s"Receiver must be a coroutine.\n" +
        //     s"required: Coroutine[${implicitly[WeakTypeTag[T]]}]\n" +
        //     s"found:    ${r.tpe} (with underlying type ${r.tpe.widen})")
        (r, args)
      case q"$r.apply[..$_](..$args)($_)" =>
        // if (!isCoroutineDefSugar(r.tpe))
        //   c.abort(r.pos,
        //     s"Receiver must be a coroutine.\n" +
        //     s"required: Coroutine[${implicitly[WeakTypeTag[T]]}]\n" +
        //     s"found:    ${r.tpe} (with underlying type ${r.tpe.widen})")
        (r, args)
      case _ =>
        c.abort(
          tree.pos,
          "The call statement must take a coroutine invocation expression:\n" +
          "  call(<coroutine>.apply(<arg0>, ..., <argN>))")
    }

    //val tpe = implicitly[WeakTypeTag[T]]
    val t = q"""
      $receiver.$$call(..$args)
    """
    t
  }
}
