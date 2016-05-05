package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import org.scalameter.japi.annotation._
import org.scalameter.picklers.noPickler._
import org.scalameter.execution.invocation._



class CoroutineBoxingBench extends JBench.Forked[Long] {
  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> 2,
    exec.maxWarmupRuns -> 5,
    exec.independentSamples -> 1,
    verbose -> false
  )

  def measurer: Measurer[Long] =
    for (table <- Measurer.BoxingCount.allWithoutBoolean()) yield {
      table.copy(value = table.value.valuesIterator.sum)
    }

  def aggregator: Aggregator[Long] = Aggregator.median

  override def reporter = Reporter.Composite(
    LoggingReporter(),
    ValidationReporter()
  )

  val sizes = Gen.single("size")(1000)

  val rangeCtx = Context(
    reports.validation.predicate -> { (n: Any) => n == 1 }
  )

  @gen("sizes")
  @benchmark("coroutines.range")
  @curve("Range")
  @ctx("rangeCtx")
  def range(sz: Int) {
    val id = coroutine { (n: Int) =>
      var i = 0
      while (i < n) {
        yieldval(i)
        i += 1
      }
    }

    var i = 0
    val c = call(id(sz))
    while (i < sz) {
      c.resume
      c.value
      i += 1
    }
  }
}
