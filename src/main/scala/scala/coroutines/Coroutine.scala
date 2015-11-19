package scala.coroutines



import scala.coroutines.common.Stack
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



class Coroutine[T] {
  var costackptr = 0
  var costack = new Array[Coroutine.Definition[T]](Coroutine.INITIAL_STACK_SIZE)

  final def push(cd: Coroutine.Definition[T]) {
    Stack.push(costack, cd)
    cd.push(this)
  }

  final def pop() {
    val cd = Stack.pop(costack)
    cd.pop(this)
  }
}


object Coroutine {
  private[coroutines] val INITIAL_STACK_SIZE = 8

  abstract class Definition[T] {
    def push(c: Coroutine[T]): Unit
    def pop(c: Coroutine[T]): Unit
  }

  private def inferReturnType(c: Context)(body: c.Tree): c.Tree = {
    import c.universe._

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

  def transform(c: Context)(f: c.Tree): c.Tree = {
    import c.universe._

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
    val rettpe = inferReturnType(c)(body)

    // emit coroutine construction
    val coroutineTpe = TypeName(s"Arity${args.size}")
    val co = q"""new scala.coroutines.Coroutine.$coroutineTpe[..$argtpes, $rettpe] {
      def apply(..$args) = {
        new Coroutine[$rettpe]
      }
      def push(c: Coroutine[$rettpe]) = {
        ???
      }
      def pop(c: Coroutine[$rettpe]) = {
        ???
      }
    }"""
    co
  }

  def resume[T: c.WeakTypeTag](c: Context)(co: c.Tree): c.Tree = {
    import c.universe._

    q"()"
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
