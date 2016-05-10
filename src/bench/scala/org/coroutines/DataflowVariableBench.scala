package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.async.Async.async
import scala.async.Async.await
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



class DataflowVariableBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 100,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 36,
    exec.independentSamples -> 4,
    verbose -> true
  )

  val sizes = Gen.range("size")(5000, 25000, 5000)

  class FutureDataflowVar[T]() {
    def apply(): T = ???
    def :=(x: T): Unit = ???
  }

  @gen("sizes")
  @benchmark("coroutines.dataflow.producer-consumer")
  @curve("futures")
  def futureProducerConsumer(sz: Int) = {
    
  }
}
