package org.coroutines.extra



import org.coroutines._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Success, Failure }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



object AsyncAwait {
  /** A wrapper class for a variable of type `T`.
   *
   *  Used inside `await` to wrap the results of asynchronous computations.
   */
  class Cell[+T] {
    var x: T @uncheckedVariance = _
  }

  /** Await the result of a future.
   *
   *  When called inside an `async` body, this function will block until its
   *  associated future completes.
   *
   *  Note that the usage of `Cell` gives users the option to not directly
   *  return the result of the future.
   *
   *  @return A coroutine that yields a tuple. `async` will assign this tuple's
   *          second element to hold the completed result of the `Future` passed
   *          into the coroutine. The coroutine will directly return the
   *          result of the future.
   */
  def await[R]: Future[R] ~~> ((Future[R], Cell[R]), R) =
    coroutine { (f: Future[R]) =>
      val cell = new Cell[R]
      yieldval((f, cell))
      cell.x
    }

  /** Calls `body`, blocking on any calls to `await`.
   *
   *  @param  body A coroutine to be invoked.
   *  @return A `Future` wrapping the result of the coroutine. The future fails
   *          if `body.hasException` or if one of `body`'s yielded tuples
   *          has a failing future.
   */
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

  /** Wraps `body` inside a coroutine and asynchronously invokes it using
   * `asyncMacro`.
   *
   *  @param  body The block of code to wrap inside an asynchronous coroutine.
   *  @return A `Future` wrapping the result of `body`.
   */
  def async[Y, R](body: =>R): Future[R] = macro asyncMacro[Y, R]

  /** Implements `async`.
   *
   *  Wraps `body` inside a coroutine and calls `asyncCall`.
   *
   *  @param  body The function to be wrapped in a coroutine.
   *  @return A tree that contains an invocation of `asyncCall` on a coroutine
   *          with `body` as its body.
   */
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
