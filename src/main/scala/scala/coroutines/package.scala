package scala



import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



package object coroutines {

  @implicitNotFound(
    "To create a coroutine, use the call(<coroutine>(<arguments>)) expression " +
    "instead of invoking the coroutine definition's call method directly.")
  sealed trait CanCall

  object Permission {
    implicit val canCall = new CanCall {}
  }

  def yieldval[T](x: T): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def yieldto[T](f: Coroutine[T]): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def call[T](f: T): Coroutine[T] = macro Coroutine.call[T]

  def coroutine[T](f: Any): Coroutine.Definition[T] = macro Coroutine.synthesize

}
