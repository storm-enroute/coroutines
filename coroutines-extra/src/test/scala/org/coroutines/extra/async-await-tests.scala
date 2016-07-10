package org.coroutines.extra



import org.coroutines._
import org.scalatest._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.{ reflectiveCalls, postfixOps }
import scala.util.Success



class TestException extends Throwable


class AsyncAwaitTest extends FunSuite with Matchers {
  import AsyncAwait._

  /** Source: https://git.io/vorXv
   *  The use of Async/Await as opposed to pure futures allows this control flow
   *  to be written more easily.
   *  The execution blocks when awaiting for the result of `f1`. `f2` only blocks
   *  after `AsyncAwait.await(f1)` evaluates to `true`.
   */
  test("simple test") {
    val future = async {
      val f1 = Future(true)
      val f2 = Future(42)
      if (await(f1)) {
        await(f2)
      } else {
        0
      }
    }
    assert(Await.result(future, 1 seconds) == 42)
  }

  /** Asynchronous blocks of code can be defined either outside of or within any
   *  part of an `async` block. This allows the user to avoid triggering the
   *  computation of slow futures until it is necessary.
   *  For instance, computation will not begin on `innerFuture` until
   *  `await(trueFuture)` evaluates to true.
   */
  test("nested async blocks") {
    val outerFuture = async {
      val trueFuture = Future { true }
      if (await(trueFuture)) {
        val innerFuture = async {
          await(Future { 100 } )
        }
        await(innerFuture)
      } else {
        200
      }
    }
    assert(Await.result(outerFuture, 1 seconds) == 100)
  }

  /** Uncaught exceptions thrown inside async blocks cause the associated futures
   *  to fail.
   */
  test("error handling test 1") {
    val errorMessage = "System error!"
    val exception = intercept[RuntimeException] {
      val future = async {
        sys.error(errorMessage)
        await(Future("dog"))
      }
      val result = Await.result(future, 1 seconds)
    }
    assert(exception.getMessage == errorMessage)
  }

  test("error handling test 2") {
    val errorMessage = "Internal await error"
    val exception = intercept[RuntimeException] {
      val future = async {
        await(Future {
          sys.error(errorMessage)
          "Here ya go"
        })
      }
      val result = Await.result(future, 1 seconds)
    }
    assert(exception.getMessage == errorMessage)
  }

  /** Source: https://git.io/vowde
   *  Without the closing `()`, the compiler complains about expecting return
   *  type `Future[Unit]` but finding `Future[Nothing]`.
   */
  test("uncaught exception within async after await") {
    val future = async {
      await(Future(()))
      throw new TestException
      ()
    }
    intercept[TestException] {
      Await.result(future, 1 seconds)
    }
  }

  // Source: https://git.io/vowdk
  test("await failing future within async") {
    val base = Future[Int] { throw new TestException }
    val future = async {
      val x = await(base)
      x * 2
    }
    intercept[TestException] { Await.result(future, 1 seconds) }
  }

  /** Source: https://git.io/vowdY
   *  Exceptions thrown inside `await` calls are properly bubbled up. They cause
   *  the async block's future to fail.
   */
  test("await failing future within async after await") {
    val base = Future[Any] { "five!".length }
    val future = async {
      val a = await(base.mapTo[Int])
      val b = await(Future { (a * 2).toString }.mapTo[Int])
      val c = await(Future { (7 * 2).toString })
      b + "-" + c
    }
    intercept[ClassCastException] {
      Await.result(future, 1 seconds)
    }
  }

  test("nested failing future within async after await") {
    val base = Future[Any] { "five!".length }
    val future = async {
      val a = await(base.mapTo[Int])
      val b = await(
        await(Future((Future { (a * 2).toString }).mapTo[Int])))
      val c = await(Future { (7 * 2).toString })
      b + "-" + c
    }
    intercept[ClassCastException] {
      Await.result(future, 1 seconds)
    }
  }
}
