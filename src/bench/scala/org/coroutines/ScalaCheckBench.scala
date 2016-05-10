package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random



class ScalaCheckBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 100,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 36,
    exec.independentSamples -> 4,
    verbose -> true
  )

  val sizes = Gen.range("size")(5000, 25000, 5000)

  case class Fract(num: Int, den: Int)

  val max = 1000

  def add(a: Fract, b: Fract) = Fract(a.num * b.den + a.den * b.num, a.den * b.den)

  trait Gen[T] {
    self =>
    def sample: T
    def map[S](f: T => S): Gen[S] = new Gen[S] {
      def sample = f(self.sample)
    }
    def flatMap[S](f: T => Gen[S]): Gen[S] = new Gen[S] {
      def sample = f(self.sample).sample
    }
  }

  def ints(from: Int, until: Int) = new Gen[Int] {
    val random = new Random
    def sample = from + random.nextInt(until - from)
  }

  @gen("sizes")
  @benchmark("coroutines.scalacheck.fractions")
  @curve("scalacheck")
  def scalacheckTestFraction(numTests: Int) = {
    val fracts = for {
      den <- ints(1, max)
      num <- ints(0, den)
    } yield Fract(num, den)
    val pairs = for {
      a <- fracts
      b <- fracts
    } yield (a, b)
    for (i <- 0 until numTests) {
      val (a, b) = pairs.sample
      val c = add(a, b)
      assert(c.num < 2 * c.den, c)
    }
  }

  class Backtracker {
    val random = new Random

    val recurse: (Unit <~> Unit) ~~> (Unit, Unit) = coroutine { (c: Unit <~> Unit) =>
      if (c.resume) {
        val saved = c.snapshot
        recurse(c)
        recurse(saved)
      } else {
        yieldval(())
      }
    }

    val traverse = coroutine { (snippet: ~~~>[Unit, Unit]) =>
      while (true) {
        val c = call(snippet())
        recurse(c)
      }
    }

    def backtrack(snippet: ~~~>[Unit, Unit], numTests: Int): Unit = {
      var testsLeft = numTests
      val t = call(traverse(snippet))
      for (i <- 0 until numTests) t.resume
    }

    val int = coroutine { (from: Int, until: Int) =>
      yieldval(())
      from + random.nextInt(until - from)
    }
  }

  @gen("sizes")
  @benchmark("coroutines.scalacheck.fractions")
  @curve("coroutine")
  def coroutineTestFraction(numTests: Int) = {
    val b = new Backtracker
    val fract = coroutine { () =>
      val den = b.int(1, max)
      val num = b.int(0, den)
      Fract(num, den)
    }
    var i = 0
    val test = coroutine { () =>
      val a = fract()
      val b = fract()
      val c = add(a, b)
      i += 1
      assert(c.num < 2 * c.den)
    }
    b.backtrack(test, numTests)
  }
}
