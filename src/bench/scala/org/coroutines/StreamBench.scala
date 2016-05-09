package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class StreamBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 50,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 36,
    exec.independentSamples -> 4,
    verbose -> true
  )

  val fibSizes = Gen.range("size")(5000, 25000, 5000)

  val taylorSizes = Gen.range("size")(50000, 250000, 50000)

  @gen("fibSizes")
  @benchmark("coroutines.stream.fibonacci.to-buffer")
  @curve("stream")
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

  @gen("fibSizes")
  @benchmark("coroutines.stream.fibonacci.to-buffer")
  @curve("coroutine")
  def coroutineFibonacciToBuffer(sz: Int) = {
    val buffer = mutable.Buffer[BigInt]()
    val fibs = coroutine { () =>
      var prev = BigInt(0)
      var curr = BigInt(1)
      yieldval(prev)
      yieldval(curr)
      while (true) {
        val x = curr + prev
        yieldval(x)
        prev = curr
        curr = x
      }
    }
    var i = 0
    val c = call(fibs())
    while (i < sz) {
      c.resume
      buffer += c.value
      i += 1
    }
    buffer
  }

  @gen("taylorSizes")
  @benchmark("coroutines.stream.taylor.sum")
  @curve("stream")
  def streamTaylorSum(sz: Int) = {
    var sum = 0.0
    class TaylorInvX(x: Double) {
      lazy val values: Stream[Double] =
        1.0 #:: values.map(_ * (x - 1) * -1)
    }
    var i = 0
    var s = new TaylorInvX(0.5).values
    while (i < sz) {
      sum += s.head
      s = s.tail
      i += 1
    }
    sum
  }

  @gen("taylorSizes")
  @benchmark("coroutines.stream.taylor.sum")
  @curve("coroutine")
  def coroutineTaylorSum(sz: Int) = {
    var sum = 0.0
    val taylor = coroutine { (x: Double) =>
      var last = 1.0
      yieldval(last)
      while (true) {
        last *= -1.0 * (x - 1)
        yieldval(last)
      }
    }
    var i = 0
    val c = call(taylor(0.5))
    while (i < sz) {
      c.resume
      sum += c.value
      i += 1
    }
    sum
  }

}
