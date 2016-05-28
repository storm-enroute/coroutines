package org.coroutines



import org.scalatest._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



object AsyncAwaitTest {
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

  object ToughTypeObject {
    class Inner

    def m2 = async(coroutine { () =>
      val y = await { Future[List[_]] { Nil } }
      val z = await { Future[Inner] { new Inner } }
      (y, z)
    })
  }
}


class AsyncAwaitTest extends FunSuite with Matchers {
  // Source: https://git.io/vrHtj
  test("propagates tough types") {
    val fut = org.coroutines.AsyncAwaitTest.ToughTypeObject.m2
    val res: (List[_], org.coroutines.AsyncAwaitTest.ToughTypeObject.Inner) =
      Await.result(fut, 2 seconds)
    assert(res._1 == Nil)
  }

  // Source: https://git.io/vrHmG
  // NOTE: Currently fails compilation
  test("pattern matching partial function") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      AsyncAwaitTest.await { Future { 1 } }
      val a = AsyncAwaitTest.await { Future { 1 } }
      val f = { case x => x + a }: PartialFunction[Int, Int]
      AsyncAwaitTest.await { Future { f(2) } }
    })
    val res = Await.result(c, 2 seconds)
    assert(res == 3)
  }
}
