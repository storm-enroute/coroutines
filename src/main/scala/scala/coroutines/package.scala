package scala



import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



package object coroutines {

  def yieldval[T](x: T): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def yieldto[T](f: Coroutine[T]): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def resume[T](co: Coroutine[T]): T = macro Coroutine.resume[T]

  def coroutine[T](f: Any): Coroutine.Definition[T] = macro Coroutine.transform

}
