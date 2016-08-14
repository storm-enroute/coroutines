package org.coroutines.extra



import org.coroutines._
import org.scalameter.api._
import org.scalameter.japi.JBench
import org.scalameter.picklers.noPickler._
import org.scalameter.execution.invocation._



class EnumeratorsBoxingBench extends JBench.Forked[Long] {
  override def defaultConfig = Context(
    exec.minWarmupRuns -> 2,
    exec.maxWarmupRuns -> 5,
    exec.independentSamples -> 1,
    verbose -> false
  )

  def measurer =
    for (table <- Measurer.BoxingCount.allWithoutBoolean()) yield {      
      table.copy(value = table.value.valuesIterator.sum)
    }

  def aggregator = Aggregator.median

  override def reporter = Reporter.Composite(
    LoggingReporter(),
    ValidationReporter()
  )

  val sizes = Gen.single("size")(1000)

  val noBoxingContext = Context(
    reports.validation.predicate -> { (n: Any) => n == 0 }
  )

  @gen("sizes")
  @benchmark("coroutines.extra.boxing.apply.instance.noReturn")
  @curve("coroutine")
  @ctx("noBoxingContext")
  def applyInstanceTestNoReturn(size: Int) {
    val id = coroutine { (n: Int) =>
      var i = 0
      while (i < n) {
        yieldval(i)
        i += 1
      }
    }
    var i = 0
    val instance = call(id(size))
    val enumerator = Enumerator(instance)
  }

  @gen("sizes")
  @benchmark("coroutines.extra.boxing.apply.coroutine_0.noReturn")
  @curve("coroutine")
  @ctx("noBoxingContext")
  def applyCoroutine_0TestNoReturn(size: Int) {
    val rube = coroutine { () =>
      yieldval(1)
      yieldval(2)
      yieldval(3)
    }
    val enumerator = Enumerator(rube)
  }

  @gen("sizes")
  @benchmark("coroutines.extra.boxing.apply.instance.return")
  @curve("coroutine")
  @ctx("noBoxingContext")
  def applyInstanceTestReturn(size: Int) {
    val idDifferentReturnType = coroutine { (n: Int) =>
      var i = 0
      while (i < n) {
        yieldval(i)
        i += 1
      }
      "foo"
    }

    val idSameReturnType = coroutine { (n: Int) =>
      var i = 0
      while (i < n) {
        yieldval(i)
        i += 1
      }
      5
    }

    var i = 0
    val foo = Enumerator(call(idDifferentReturnType(size)))
    val bar = Enumerator(call(idSameReturnType(size)))
  }

  @gen("sizes")
  @benchmark("coroutines.extra.boxing.apply.coroutine_0.return")
  @curve("coroutine")
  @ctx("noBoxingContext")
  def applyCoroutine_0TestReturn(size: Int) {
    val rubeDifferentReturnType = coroutine { () =>
      yieldval(1)
      yieldval(2)
      yieldval(3)
      "bar"
    }
    val rubeSameReturnType = coroutine { () =>
      yieldval(1)
      yieldval(2)
      yieldval(3)
      1
    }
    val foo = Enumerator(rubeDifferentReturnType)
    val bar = Enumerator(rubeSameReturnType)
  }
}
