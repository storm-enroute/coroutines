package org.examples



import org.coroutines._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



object AsyncAwait {
  class Cell[+T] {
    var x: T @uncheckedVariance = _
  }

  /**
    * The future should be computed after the pair is yielded. The result of
    * this future can be used to assign a value to cell.x.
    * Note that Cell is used in order to give users the option to not directly
    * return the result of the future.
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
      if (!c.resume) p.success(c.result)
      else {
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

  def main(args: Array[String]) {
    val f = Future { math.sqrt(121) }
    val g = Future { math.abs(-15) }
    // Calls to yieldval inside an inner coroutine are yield points inside the
    // outer coroutine
    val h = async(coroutine { () =>
      val x = await { f }
      val y = await { g }
      x + y
    })

    val res = scala.concurrent.Await.result(h, 5.seconds)
    assert(res == 26.0)
  }
}
