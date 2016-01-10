package org.separatepackage



import org.coroutines._
import org.scalatest._
import scala.util.Failure



class SeparatePackageTest extends FunSuite with Matchers {
  test("should declare and run a coroutine") {
    val rube = coroutine { (x: Int) =>
      yieldval(x * 2)
      if (x > 0) yieldval(x)
      else yieldval(-x)
      x + 1
    }

    val c0 = call(rube(2))
    assert(c0.resume)
    assert(c0.value == 4)
    assert(c0.resume)
    assert(c0.value == 2)
    assert(!c0.resume)
    assert(c0.result == 3)
    assert(c0.isCompleted)

    val c1 = call(rube(-2))
    assert(c1.resume)
    assert(c1.value == -4)
    assert(c1.resume)
    assert(c1.value == 2)
    assert(!c1.resume)
    assert(c1.result == -1)
    assert(c1.isCompleted)
  }

  test("Another coroutine must be invoked without syntax sugar") {
    val inc = coroutine { (x: Int) => x + 1 }
    val rube = coroutine { () =>
      inc(3)
    }

    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 4)
    assert(c.isCompleted)
  }
}
