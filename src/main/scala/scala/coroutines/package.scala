package scala



import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



package object coroutines {

  def yieldval[T](x: T): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def yieldto[T](f: Coroutine.Frame[T]): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def coroutine[T](f: Any): Coroutine[T] = macro Coroutine.transformation

}
