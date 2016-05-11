package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.async.Async.async
import scala.async.Async.await
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



class AsyncAwaitBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 100,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 36,
    exec.independentSamples -> 4,
    verbose -> false
  )

  val sizes = Gen.range("size")(5000, 25000, 5000)

  private def request(i: Int): Future[Unit] = Future { () }

  @gen("sizes")
  @benchmark("coroutines.async.request-reply")
  @curve("async")
  def asyncAwait(sz: Int) = {
    val done = async {
      var i = 0
      while (i < sz) {
        val reply = await(request(i))
        i += 1
      }
    }
    Await.result(done, 10.seconds)
  }

  def coroutineAsync[Y, T](f: Coroutine._0[Future[Y], T]): Future[T] = {
    val c = call(f())
    val p = Promise[T]()
    def loop() {
      if (!c.resume) p.success(c.result)
      else c.value.onComplete {
        case _ => loop()
      }
    }
    Future { loop() }
    p.future
  }

  def coroutineAwait[T]: Coroutine._1[Future[T], Future[T], T] = coroutine {
    (f: Future[T]) =>
    yieldval(f)
    f.value.get.get
  }

  @gen("sizes")
  @benchmark("coroutines.async.request-reply")
  @curve("coroutine")
  def coroutineAsyncAwait(sz: Int) = {
    val done = coroutineAsync {
      coroutine { () =>
        var i = 0
        while (i < sz) {
          val reply = coroutineAwait(request(i))
          i += 1
        }
      }
    }
    Await.result(done, 10.seconds)
  }
}
