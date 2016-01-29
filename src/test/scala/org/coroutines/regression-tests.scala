package org.coroutines



import org.scalatest._
import scala.util.Failure



class RegressionTest extends FunSuite with Matchers {
  test("should declare body with if statement") {
    val xOrY = coroutine { (x: Int, y: Int) =>
      if (x > 0) {
        yieldval(x)
      } else {
        yieldval(y)
      }
    }
    val c1 = call(xOrY(5, 2))
    assert(c1.resume)
    assert(c1.value == 5)
    assert(!c1.resume)
    assert(c1.isCompleted)
    assert(c1.result == (()))
    val c2 = call(xOrY(-2, 7))
    assert(c2.resume)
    assert(c2.value == 7)
    assert(!c2.resume)
    assert(c2.isCompleted)
    assert(c2.result == (()))
  }

  test("coroutine should have a nested if statement") {
    val numbers = coroutine { () =>
      var z = 1
      var i = 1
      while (i < 5) {
        if (z > 0) {
          yieldval(z * i)
          z = -1
        } else {
          yieldval(z * i)
          z = 1
          i += 1
        }
      }
    }
    val c = call(numbers())
    for (i <- 1 until 5) {
      assert(c.resume)
      assert(c.value == i)
      assert(c.resume)
      assert(c.value == -i)
    }
  }

  test("coroutine should call a coroutine with a different return type") {
    val stringer = coroutine { (x: Int) => x.toString }
    val caller = coroutine { (x: Int) =>
      val s = stringer(2 * x)
      yieldval(s)
      x * 3
    }

    val c = call(caller(5))
    assert(c.resume)
    assert(c.value == "10")
    assert(!c.resume)
    assert(c.result == 15)
  }

  test("issue #14 -- simple case") {
    object Test {
      val foo: Int ~~> (Int, Unit) = coroutine { (i: Int) =>
        yieldval(i)
        if (i > 0) {
          foo(i - 1)
          foo(i - 1)
        }
      }
    }

    val c = call(Test.foo(2))
    assert(c.resume)
    assert(c.value == 2)
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 0)
    assert(!c.resume)
  }

  test("issue #14 -- complex case") {
    object Test {
      val foo: Int ~~> (Int, Unit) = coroutine { (i: Int) =>
        yieldval(i)
        if (i > 0) {
          foo(i - 1)
          foo(i - 1)
        }
      }
    }

    val bar = coroutine { () =>
      Test.foo(2)
      Test.foo(2)
    }

    val c = call(bar())
    assert(c.resume)
    assert(c.value == 2)
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 2)
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 0)
    assert(c.resume)
    assert(c.value == 0)
    assert(!c.resume)
  }

  test("issue #15 -- hygiene") {
    val scala, Any, String, TypeTag, Unit = ()
    trait scala; trait Any; trait String; trait TypeTag; trait Unit

    val id = coroutine { (x: Int) =>
      x
    }
  }

  test("issue #15 -- more hygiene") {
    val org, coroutines, Coroutine = ()
    trait org; trait coroutines; trait Coroutine
    
    val id = coroutine { () => }
  }
}
