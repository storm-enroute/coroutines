package org.coroutines



import org.scalatest._
import scala.collection._
import scala.util.Failure



class YieldToTest extends FunSuite with Matchers {
  test("after resuming to another coroutine, there should be no value") {
    val another = coroutine { () =>
      yieldval("Yohaha")
    }
    val anotherInstance = call(another())

    val rube = coroutine { () =>
      yieldval("started")
      yieldto(anotherInstance)
    }

    val c = call(rube())
    assert(c.resume)
    assert(c.hasValue)
    assert(c.value == "started")
    assert(c.resume)
    assert(!c.hasValue)
    assert(!c.resume)
    assert(!c.hasValue)
    assert(c.isCompleted)
  }

  test("yielding to a completed coroutine raises an error") {
    val another = coroutine { () => "in and out" }
    val anotherInstance = call(another())
    assert(!anotherInstance.resume)

    val rube = coroutine { () =>
      yieldto(anotherInstance)
      yieldval("some more")
    }
    val c = call(rube())
    assert(!c.resume)
    c.tryResult match {
      case Failure(e: CoroutineStoppedException) =>
      case _ => assert(false, "Should have thrown an exception.")
    }
  }

  test("should be able to yield to a differently typed coroutine") {
    val another: ~~~>[String, Unit] = coroutine { () =>
      yieldval("hohoho")
    }
    val anotherInstance = call(another())

    val rube: Int ~~> (Int, Int) = coroutine { (x: Int) =>
      yieldval(-x)
      yieldto(anotherInstance)
      x
    }
    val c = call(rube(5))

    assert(c.resume)
    assert(c.value == -5)
    assert(c.resume)
    assert(!c.hasValue)
    assert(!c.resume)
    assert(c.result == 5)
  }

  test("should drain the coroutine instance that yields to another coroutine") {
    val another: ~~~>[String, Unit] = coroutine { () =>
      yieldval("uh-la-la")
    }
    val anotherInstance = call(another())

    val rube: (Int, Int) ~> (Int, Unit) = coroutine { (x: Int, y: Int) =>
      yieldval(x)
      yieldval(y)
      yieldto(anotherInstance)
      yieldval(x * y)
    }
    val c = call(rube(5, 4))

    val b = mutable.Buffer[Int]()
    while (c.resume) if (c.hasValue) b += c.value

    assert(b == Seq(5, 4, 20))
  }
}
