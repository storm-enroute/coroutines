package org.coroutines.extra



import org.coroutines._
import org.scalatest._
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._



class AsyncAwaitTests extends FunSuite with Matchers {
  class TestException extends Throwable

  test("simple test defined in Scala Async") {
    val future = AsyncAwait.async(coroutine { () => 
      val f1 = AsyncAwait.async(coroutine { () =>
        AsyncAwait.await(Future(true))
      })
      val f2 = AsyncAwait.async(coroutine { () =>
        AsyncAwait.await(Future(42))
      })
      if (AsyncAwait.await(f1))
        AsyncAwait.await(f2) 
      else
        0
    })
    val result = Await.result(future, 1 seconds)
    assert(result == 42)
  }

  test("error handling test 1") {
    val errorMessage = "System error!"
    val exception = intercept[RuntimeException] {
      val c = coroutine { () =>
        sys.error(errorMessage)
        AsyncAwait.await(Future("dog"))
      }
      val future = AsyncAwait.async(c)
      val result = Await.result(future, 1 seconds)
    }
    assert(exception.getMessage == errorMessage)
  }

  test("error handling test 2") {
    intercept[TestException] {
      val c = coroutine { () =>
        throw new TestException
        yieldval((Future("god"), new AsyncAwait.Cell[String]))
        AsyncAwait.await(Future("dog"))
      }
      val future = AsyncAwait.async(c)
      Await.result(future, 1 seconds)
    }
  }

  // Source: https://git.io/vowde
  test("uncaught exception within async after await") {
    val future = AsyncAwait.async(coroutine { () =>
      AsyncAwait.await(Future(()))
      throw new TestException
    })
    intercept[TestException] { Await.result(future, 1 seconds) }
  }

  // Source: https://git.io/vowdk
  test("await failing future within async") {
    val base = Future[Int] { throw new TestException }
    val future = AsyncAwait.async(coroutine { () =>
      val x = AsyncAwait.await(base)
      x * 2
    })
    intercept[TestException] { Await.result(future, 1 seconds) }
  }

  // Source: https://git.io/vowdY
  test("await failing future within async after await") {
    val base = Future[Any] { "five!".length }
    val future = AsyncAwait.async(coroutine { () =>
      val a = AsyncAwait.await(base.mapTo[Int])
      val b = AsyncAwait.await(Future { (a * 2).toString }.mapTo[Int])
      val c = AsyncAwait.await(Future { (7 * 2).toString })
      b + "-" + c
    })
    intercept[ClassCastException] {
      Await.result(future, 1 seconds)
    }
  }

  test("nested failing future within async after await") {
    val base = Future[Any] { "five!".length }
    val future = AsyncAwait.async(coroutine { () =>
      val a = AsyncAwait.await(base.mapTo[Int])
      val b = AsyncAwait.await(AsyncAwait.await(Future((Future { (a * 2).toString }).mapTo[Int])))
      val c = AsyncAwait.await(Future { (7 * 2).toString })
      b + "-" + c
    })
    intercept[ClassCastException] {
      Await.result(future, 1 seconds)
    }
  }
}
