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
    val h = async(coroutine { () =>
      val x = await { f }
      val y = await { g }
      x + y
    })

    val res = scala.concurrent.Await.result(h, 5.seconds)
    assert(res == 26.0)
  }
}
