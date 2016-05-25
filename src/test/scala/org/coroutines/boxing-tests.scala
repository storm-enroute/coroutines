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

  /**
    * Range Iterator
    * No values should be boxed because Synthesizer.specArity1 automatically
    * specializes the coroutine on the yield type. In addition, Coroutine._1 is
    * specialized on the input argument type. 
    */
  val rangeCtx = Context(
    reports.validation.predicate -> { (n: Any) => n == 0 }
  )

  @gen("sizes")
  @benchmark("coroutines.boxing.range")
  @curve("coroutine")
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

  /**
    * Tree Iterator
    * Ensures that no values are boxed when they are put inside a binary tree
    * and traversed in-order 
    */
  val treeCtx = Context(
    reports.validation.predicate -> { (n: Any) => n == 0 }
  )

  sealed trait Tree
  case class Node(x: Int, left: Tree, right: Tree) extends Tree
  case object Empty extends Tree

  var iterator: Coroutine._1[Tree, Int, Unit] = _

  @gen("sizes")
  @benchmark("coroutines.boxing.tree-iterator")
  @curve("coroutine")
  @ctx("treeCtx")
  def tree(sz: Int) {
    def gen(sz: Int): Tree = {
      if (sz == 0) Empty
      else { 
        val rem = sz - 1
        val left = gen(rem / 2)
        val right = gen(rem - rem / 2)
        Node(sz, left, right)
      }
    }
    val tree = gen(sz)

    iterator = coroutine { (t: Tree) =>
      t match {
        case n: Node =>
          iterator(n.left)
          yieldval(n.x)
          iterator(n.right)
        case Empty =>
      }
    }

    val c = call(iterator(tree))
    while (c.pull) c.value
  }

  /**
    * Fibonacci
    * There should be only one boxing call made. It comes from the final
    * return value because _1$spec$I is not specialized on the coroutine return
    * type.
    */
  val fibCtx = Context(
    reports.validation.predicate -> { (n: Any) => n == 1 }
  )

  val fibSizes = Gen.single("size")(10)

  @gen("fibSizes")
  @benchmark("coroutines.boxing.fibonacci")
  @curve("coroutine")
  @ctx("fibCtx")
  def fibonacci(sz: Int) {
    var fib: _1$spec$I[Unit, Int] = null
    fib = coroutine { (n: Int) =>
      if (n <= 1) 1
      else fib(n - 1) + fib(n - 2)
    }
    val c = call(fib(sz))
    while (c.pull) c.value
  }

  /**
    * Fibonnaci Sugar
    * There should boxing call made for each instantiation of fibsugar. It 
    * happens because the type Int ~~> (Unit, Int) is not specialized on the
    * argument type. In addition, there is one boxing call for the final return.
    */

  val fibSugarCtx = Context(
    reports.validation.predicate -> { (n: Any) => n == 178 }
  )

  @gen("fibSizes")
  @benchmark("coroutines.boxing.fibonacci")
  @curve("coroutine-sugar")
  @ctx("fibSugarCtx")
  def fibonacciSugar(sz: Int) {
    var fibsugar: Int ~~> (Unit, Int) = null
    fibsugar = coroutine { (n: Int) =>
      if (n <= 1) 1
      else fibsugar(n - 1) + fibsugar(n - 2)
    }
    val cs = call(fibsugar(sz))
    while (cs.pull) cs.value
  }
}
