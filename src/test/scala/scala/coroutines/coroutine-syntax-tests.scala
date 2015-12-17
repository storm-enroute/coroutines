package scala.coroutines



import org.scalatest._
import scala.util.Failure



class CoroutineSyntaxTest extends FunSuite with Matchers {
  test("Coroutine._0 must be invoked") {
    val rube = coroutine { () =>
      yieldval(5)
      yieldval(-5)
      10
    }
    val co: ~~~>[Int] = rube
    val c = call(co())
    assert(c() == 5)
    assert(c() == -5)
    assert(c() == 10)
    assert(c.isStopped)
  }

  test("Coroutine._1 must be invoked") {
    val rube = coroutine { (x: Int) =>
      yieldval(x + x)
      yieldval(x - 2 * x)
      x
    }

    val co: Int ~~> Int = rube
    val c = call(co(7))
    assert(c() == 14)
    assert(c() == -7)
    assert(c() == 7)
    assert(c.isStopped)
  }

  test("Coroutine._1 must be invoked for tuples with the quoting type") {
    val rube = coroutine { (t: (Int, String)) =>
      yieldval(t._1)
      t._2
    }

    val co: (Int, String) ~~> Any = rube
    val c = call(co((7, "ok")))
    assert(c() == 7)
    assert(c() == "ok")
    assert(c.isStopped)
  }

  test("Coroutine._2 must be invoked") {
    val rube = coroutine { (x: Int, y: Int) =>
      yieldval(x + y)
      yieldval(x - y)
      x * y
    }

    val co: (Int, Int) ~> Int = rube
    val c = call(co(7, 4))
    assert(c() == 11)
    assert(c() == 3)
    assert(c() == 28)
    assert(c.isStopped)
  }

  test("Coroutine._3 must be invoked") {
    val rube = coroutine { (x: Int, y: Int, z: Int) =>
      yieldval(x)
      yieldval(y)
      z
    }

    val co: (Int, Int, Int) ~> Int = rube
    val c = call(co(3, 5, 8))
    assert(c() == 3)
    assert(c() == 5)
    assert(c() == 8)
    assert(c.isStopped)
  }
}