package org.examples



import org.coroutines._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



object AsyncAwait {
  def await[R]: Coroutine._1[Future[R], (Future[R], Cell[R]), R] =
    coroutine { (f: Future[R]) =>
      val cell = new Cell[R]
      yieldval((f, cell))
      cell.x
    }

  def main(args: Array[String]) {
    val f = Future { math.sqrt(121) }
    val g = Future { math.abs(-15) }
    val h = async {
      val x = await { f }
      val y = await { g }
      x + y
    }

    val res = scala.concurrent.Await.result(h, 5.seconds)
    assert(res == 26.0)
  }
}
