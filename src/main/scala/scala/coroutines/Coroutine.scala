package scala.coroutines



import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



abstract class Coroutine[T] {
}


object Coroutine {
  abstract class Frame[T](val coroutine: Coroutine[T]) {
    def apply(): T
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
      case q"$qual.yieldto[$tpt]($f)" if isCoroutines(qual) => tq"$tpt" match {
        case tq"Frame[$tpt]" => tpt.tpe
        case _ => c.abort(f.pos, "The yieldto argument must be a Coroutine.Frame.")
      }
    }
    tq"${lub(rettpe :: constraintTpes)}"
  }

  def transformation(c: Context)(f: c.Tree): c.Tree = {
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
      def apply(..$args) = ???
    }"""
    println("tree: " + co)
    co
  }

  abstract class Arity0[T] extends Coroutine[T] {
    def apply(): T
  }

  abstract class Arity1[A0, T] extends Coroutine[T] {
    def apply(a0: A0): T
  }

  abstract class Arity2[A0, A1, T] extends Coroutine[T] {
    def apply(a0: A0, a1: A1): T
  }

  abstract class Arity3[A0, A1, A2, T] extends Coroutine[T] {
    def apply(a0: A0, a1: A1, a2: A2): T
  }
}
