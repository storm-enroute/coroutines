package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class RedBlackIteratorBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 40,
    exec.maxWarmupRuns -> 80,
    exec.benchRuns -> 60,
    exec.independentSamples -> 6,
    exec.reinstantiation.frequency -> 1,
    verbose -> true
  )

  val sizes = Gen.range("size")(50000, 250000, 50000)

  val trees = for (sz <- sizes) yield {
    var tree = immutable.TreeSet[String]()
    for (i <- 0 until sz) tree += i.toString
    tree
  }

  /* longest string */

  @gen("trees")
  @benchmark("coroutines.red-black-iterator.longest")
  @curve("coroutine")
  def coroutineLongest(set: immutable.TreeSet[String]) = {
    var longest = ""
    val treeIterator = Backdoor.redBlackIterator
    val tree = Backdoor.redBlack(set)
    val c = call(treeIterator(tree))
    while (c.pull) {
      val s = c.value
      if (longest.length < s.length) longest = s
    }
    longest
  }

  @gen("trees")
  @benchmark("coroutines.red-black-iterator.longest")
  @curve("iterator")
  def iteratorLongest(set: immutable.TreeSet[String]) = {
    var longest = ""
    val i = set.iterator
    while (i.hasNext) {
      val s = i.next()
      if (longest.length < s.length) longest = s
    }
    longest
  }

}
