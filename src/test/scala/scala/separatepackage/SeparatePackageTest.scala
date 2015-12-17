package scala.separatepackage



import org.scalatest._
import scala.coroutines._
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
    assert(c0() == 4)
    assert(c0() == 2)
    assert(c0() == 3)
    assert(c0.isStopped)

    val c1 = call(rube(-2))
    assert(c1() == -4)
    assert(c1() == 2)
    assert(c1() == -1)
    assert(c1.isStopped)
  }

  test("Another coroutine must be invoked without syntax sugar") {
    val id = coroutine { (x: Int) => x }
    val rube = coroutine { () =>
      id(7)
    }

    val c = call(rube())
    assert(c() == 7)
    assert(c.isStopped)
  }
}
