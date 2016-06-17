package org.coroutines.extra



import org.coroutines._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }



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

  def async[Y, R](body: ~~~>[(Future[Y], Cell[Y]), R]): Future[R] = {
    val c = call(body())
    val p = Promise[R]
    def loop() {
      if (!c.resume) {
        if (c.hasException) {
          p.failure(c.$exception)
        } else {
          p.success(c.result)
        }
      } else {
        val (future, cell) = c.value
        for (x <- future) {
          cell.x = x
          loop()
        }
      }
    }
    Future { loop() }
    p.future
  }
}
