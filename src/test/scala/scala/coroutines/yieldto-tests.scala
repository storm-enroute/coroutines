package scala.coroutines



import org.scalatest._
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
}
