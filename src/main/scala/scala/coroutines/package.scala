package scala



import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



package object coroutines {

  def coroutine(f: Any): Any = macro coroutineImpl

  def coroutineImpl(c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    println(f)
    q"()"
  }

}
