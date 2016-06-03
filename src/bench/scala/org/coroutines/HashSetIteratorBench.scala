package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class HashSetIteratorBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 40,
    exec.maxWarmupRuns -> 80,
    exec.benchRuns -> 60,
    exec.independentSamples -> 6,
    exec.reinstantiation.frequency -> 1,
    verbose -> false
  )

  val sizes = Gen.range("size")(50000, 250000, 50000)

  val hashsets = for (sz <- sizes) yield {
    var hs = mutable.HashSet[String]()
    for (i <- 0 until sz) hs += i.toString
    hs
  }

  /* longest string */

  @gen("hashsets")
  @benchmark("coroutines.hash-set-iterator.longest")
  @curve("coroutine")
  def coroutineLongest(set: mutable.HashSet[String]) = {
    var longest = ""
    val hashIterator = Backdoor.hashSetEnumerator
    val table = Backdoor.hashSet(set)
    val c = call(hashIterator(table))
    while (c.pull) {
      val s = c.value
      if (longest.length < s.length) longest = s
    }
    longest
  }

  @gen("hashsets")
  @benchmark("coroutines.hash-set-iterator.longest")
  @curve("iterator")
  def iteratorLongest(set: mutable.HashSet[String]) = {
    var longest = ""
    val i = set.iterator
    while (i.hasNext) {
      val s = i.next()
      if (longest.length < s.length) longest = s
    }
    longest
  }

  @gen("hashsets")
  @benchmark("coroutines.hash-set-iterator.longest")
  @curve("foreach")
  def foreachLongest(set: mutable.HashSet[String]) = {
    var longest = ""
    set.foreach { s =>
      if (longest.length < s.length) longest = s
    }
    longest
  }

}
