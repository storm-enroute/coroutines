package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class StreamBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 50,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 36,
    exec.independentSamples -> 1,
    verbose -> true
  )

  val sizes = Gen.range("size")(5000, 25000, 5000)

  @gen("sizes")
  @benchmark("coroutines.stream.fibonacci.to-buffer")
  @curve("iterator")
  def streamFibonacciToBuffer(sz: Int) = {
    val buffer = mutable.Buffer[BigInt]()
    object Fibs {
      lazy val values: Stream[BigInt] =
        BigInt(0) #:: BigInt(1) #:: values.zip(values.tail).map(t => t._1 + t._2)
    }
    var i = 0
    var s = Fibs.values
    while (i < sz) {
      buffer += s.head
      s = s.tail
      i += 1
    }
    buffer
  }

}
