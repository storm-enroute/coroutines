package scala.coroutines



import org.scalatest._
import scala.util.Failure



class RegressionTest extends FunSuite with Matchers {
  // test("should declare body with if statement") {
  //   val xOrY = coroutine { (x: Int, y: Int) =>
  //     if (x > 0) {
  //       yieldval(x)
  //     } else {
  //       yieldval(y)
  //     }
  //   }
  //   val c1 = call(xOrY(5, 2))
  //   assert(c1() == 5)
  //   assert(c1() == (()))
  //   assert(c1.isStopped)
  //   val c2 = call(xOrY(-2, 7))
  //   assert(c2() == 7)
  //   assert(c2() == (()))
  //   assert(c1.isStopped)
  // }

  // test("coroutine should have a nested if statement") {
  //   val numbers = coroutine { () =>
  //     var z = 1
  //     var i = 1
  //     while (i < 5) {
  //       if (z > 0) {
  //         yieldval(z * i)
  //         z = -1
  //       } else {
  //         yieldval(z * i)
  //         z = 1
  //         i += 1
  //       }
  //     }
  //   }
  //   val c = call(numbers())
  //   for (i <- 1 until 5) {
  //     assert(c() == i)
  //     assert(c() == -i)
  //   }
  // }
}