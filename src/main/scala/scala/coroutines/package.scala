package scala



import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



package object coroutines {

  val COROUTINE_DIRECT_APPLY_ERROR_MESSAGE =
    "Coroutines can only be invoked directly from within other coroutines. " +
    "Use `call(<coroutine>(<arg0>, ..., <argN>))` instead if you want to " +
    "start a new coroutine."

  @implicitNotFound(
    "To create a coroutine, use the call(<coroutine>(<arguments>)) expression " +
    "instead of invoking the coroutine definition's call method directly.")
  sealed trait CanCallInternal

  case class CoroutineStoppedException() extends Exception

  class ~>[T, @specialized S] private[coroutines] (
    val blueprint: Coroutine.Blueprint[S])

  implicit def coroutine0[@specialized S](b: Coroutine._0[S]) =
    new ~>[<>, S](b)

  implicit def coroutine1[T, @specialized S](b: Coroutine._1[T, S]) =
    new ~>[T, S](b)

  implicit def coroutine1quoted[T, @specialized S](b: Coroutine._1[T, S]) =
    new ~>[\[T], S](b)

  implicit def coroutine2[T1, T2, @specialized S](b: Coroutine._2[T1, T2, S]) =
    new ~>[Tuple2[T1, T2], S](b)

  implicit def coroutine3[T1, T2, T3, @specialized S](b: Coroutine._3[T1, T2, T3, S]) =
    new ~>[Tuple3[T1, T2, T3], S](b)

  final class <> private ()

  final class \[T] private ()

  class coroutine0ops[@specialized S](val c: <> ~> S)
  extends Coroutine.BlueprintMarker {
    def apply() = sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call()(implicit cc: CanCallInternal): Coroutine[S] =
      c.blueprint.asInstanceOf[Coroutine._0[S]].$call()
  }

  implicit def coroutine0ops[@specialized S](c: <> ~> S) =
    new coroutine0ops(c)

  class coroutine1ops[T, @specialized S](val c: T ~> S)
  extends Coroutine.BlueprintMarker {
    def apply(t: T) = sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call(t: T)(implicit cc: CanCallInternal): Coroutine[S] =
      c.blueprint.asInstanceOf[Coroutine._1[T, S]].$call(t)
  }

  implicit def coroutine1ops[T, @specialized S](c: T ~> S) =
    new coroutine1ops(c)

  class coroutine1quotedops[T, @specialized S](val c: \[T] ~> S)
  extends Coroutine.BlueprintMarker {
    def apply(t: T) = sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call(t: T)(implicit cc: CanCallInternal): Coroutine[S] =
      c.blueprint.asInstanceOf[Coroutine._1[T, S]].$call(t)
  }

  implicit def coroutine1quotedops[T, @specialized S](c: \[T] ~> S) =
    new coroutine1quotedops(c)

  object Permission {
    implicit val canCall = new CanCallInternal {}
  }

  def yieldval[T](x: T): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def yieldto[T](f: Coroutine[T]): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def call[T](f: T): Coroutine[T] = macro Coroutine.call[T]

  def coroutine[T](f: Any): Any = macro Coroutine.synthesize

}
