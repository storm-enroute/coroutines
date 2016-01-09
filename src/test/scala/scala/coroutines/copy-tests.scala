package scala.coroutines



import org.scalatest._
import scala.collection._
import scala.util.Failure



class CopyTest extends FunSuite with Matchers {
  test("coroutine instance should be copied and resumed as needed") {
    val countdown = coroutine { (n: Int) =>
      var i = n
      while (i >= 0) {
        yieldval(i)
        i -= 1
      }
    }

    val c = call(countdown(10))
    for (i <- 0 until 5) {
      assert(c.resume)
      assert(c.value == (10 - i))
    }
    val c2 = c.copy
    for (i <- 5 to 10) {
      assert(c2.resume)
      assert(c2.value == (10 - i))
    }
    for (i <- 5 to 10) {
      assert(c.resume)
      assert(c.value == (10 - i))
    }
  }
}
