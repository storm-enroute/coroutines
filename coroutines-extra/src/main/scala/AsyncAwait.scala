package org.coroutines.extra



import org.coroutines._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



object AsyncAwait {
  class Cell[+T] {
    var x: T @uncheckedVariance = _
  }

  /** The future should be computed after the pair is yielded. The result of
   *  this future can be used to assign a value to `cell.x`.
   *  Note that `Cell` is used in order to give users the option to not directly
   *  return the result of the future.
   */
  def await[R]: Future[R] ~~> ((Future[R], Cell[R]), R) =
    coroutine { (f: Future[R]) =>
      val cell = new Cell[R]
      yieldval((f, cell))
      cell.x
    }

  def asyncCall[Y, R](body: ~~~>[(Future[Y], Cell[Y]), R]): Future[R] = {
    val c = call(body())
    val p = Promise[R]
    def loop() {
      if (!c.resume) {
        c.tryResult match {
          case Success(result) => p.success(result)
          case Failure(exception) => p.failure(exception)
        }
      } else {
        val (future, cell) = c.value
        future onComplete {
          case Success(x) =>
            cell.x = x
            loop()
          case Failure(exception) =>
            p.failure(exception)
        }
      }
    }
    Future { loop() }
    p.future
  }

  def async[Y, R](body: =>R): Future[R] = macro asyncMacro[Y, R]

  def asyncMacro[Y, R](c: Context)(body: c.Tree): c.Tree = {
    import c.universe._

    q"""
       val c = coroutine { () =>
         $body
       }
       _root_.org.coroutines.extra.AsyncAwait.asyncCall(c)
     """
  }
}
