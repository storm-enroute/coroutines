package scala.coroutines



import scala.annotation.tailrec
import scala.coroutines.common.Stack
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



class Coroutine[T] {
  import Coroutine._
  private[coroutines] var costackptr = 0
  private[coroutines] var costack = new Array[Definition[T]](INITIAL_CO_STACK_SIZE)
  private[coroutines] var pcstackptr = 0
  private[coroutines] var pcstack = new Array[Short](INITIAL_CO_STACK_SIZE)
  private[coroutines] var target: Coroutine[T] = null
  private[coroutines] var result: T = null.asInstanceOf[T]

  final def push(cd: Coroutine.Definition[T]) {
    Stack.push(costack, cd)
    cd.push(this)
  }

  final def pop() {
    val cd = Stack.pop(costack)
    cd.pop(this)
  }

  @tailrec
  final def enter(): T = {
    val cd = Stack.top(costack)
    cd.enter(this)
    if (target ne null) {
      val nc = target
      target = null
      nc.enter()
    } else result
  }
}


object Coroutine {
  private[coroutines] val INITIAL_CO_STACK_SIZE = 4
  private[coroutines] val INITIAL_VAR_STACK_SIZE = 8

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

    def inferReturnType(body: Tree): Tree = {
      // return type must correspond to the return type of the function literal
      val rettpe = body.tpe

      // return type must be the lbu of the function return type and yield argument types
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

    def generateVariableMap(args: List[Tree], body: Tree): Map[Symbol, Int] = {
      Map()
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

      // generate entry method

      // emit coroutine instantiation
      val coroutineTpe = TypeName(s"Arity${args.size}")
      val co = q"""new scala.coroutines.Coroutine.$coroutineTpe[..$argtpes, $rettpe] {
        def apply(..$args) = {
          new Coroutine[$rettpe]
        }
        def push(c: Coroutine[$rettpe]): Unit = {
          ???
        }
        def pop(c: Coroutine[$rettpe]): Unit = {
          ???
        }
        def enter(c: Coroutine[$rettpe]): Unit = {
          ???
        }
      }"""
      co
    }
  }

  abstract class Arity0[T] extends Coroutine.Definition[T] {
    def apply(): Coroutine[T]
  }

  abstract class Arity1[A0, T] extends Coroutine.Definition[T] {
    def apply(a0: A0): Coroutine[T]
  }

  abstract class Arity2[A0, A1, T] extends Coroutine.Definition[T] {
    def apply(a0: A0, a1: A1): Coroutine[T]
  }

  abstract class Arity3[A0, A1, A2, T] extends Coroutine.Definition[T] {
    def apply(a0: A0, a1: A1, a2: A2): Coroutine[T]
  }
}
