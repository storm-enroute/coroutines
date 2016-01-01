package scala.coroutines



import org.scalatest._
import scala.util.Failure



class PatternMatchTest extends FunSuite with Matchers {
  test("simple pattern match") {
    val rube = coroutine { (x: AnyRef) =>
      x match {
        case s: String => s.length
        case xs: List[_] => xs.size
      }
    }

    val c1 = call(rube("ok"))
    assert(!c1.resume)
    assert(c1.result == 2)
    assert(c1.isCompleted)
    val c2 = call(rube(1 :: 2 :: 3 :: Nil))
    assert(!c2.resume)
    assert(c2.result == 3)
    assert(c2.isCompleted)
  }

  test("pattern match with yields") {
    val rube = coroutine { (x: AnyRef) =>
      x match {
        case s: String => yieldval(s.length)
        case xs: List[_] => yieldval(xs.size)
      }
      17
    }

    val c1 = call(rube("ok"))
    assert(c1.resume)
    assert(c1.value == 2)
    assert(!c1.resume)
    assert(c1.result == 17)
    assert(c1.isCompleted)
    val c2 = call(rube(1 :: 2 :: 3 :: Nil))
    assert(c2.resume)
    assert(c2.value == 3)
    assert(!c2.resume)
    assert(c2.result == 17)
    assert(c2.isCompleted)
  }
}
