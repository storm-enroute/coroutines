package org.coroutines.extra



import org.coroutines._
import org.scalatest._
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._



class AsyncAwaitTests extends FunSuite with Matchers {
  val errorMessage = "Life ain't no Nintendo game"

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
    val exception = intercept[RuntimeException] {
      val c = coroutine { () =>
        sys.error(errorMessage)
        yieldval((Future("god"), new AsyncAwait.Cell[String]))
        AsyncAwait.await(Future("dog"))
      }
      val future = AsyncAwait.async(c)
      Await.result(future, 1 seconds)
    }
    assert(exception.getMessage == errorMessage)
  }
}
