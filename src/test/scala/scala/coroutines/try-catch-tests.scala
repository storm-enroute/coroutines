package scala.coroutines



import org.scalatest._
import scala.util.Failure



class TryCatchTest extends FunSuite with Matchers {
  // test("should declare a coroutine with a try-catch block") {
  //   val rube = coroutine { () =>
  //     try {
  //       throw new Exception
  //     } catch {
  //       case e: Exception =>
  //     }
  //   }
  // }

  // test("should declare a coroutine with a try-catch-finally block") {
  //   val rube = coroutine { () =>
  //     try {
  //       throw new Exception
  //     } catch {
  //       case e: Exception =>
  //     } finally {
  //       sys.error("done")
  //     }
  //   }
  // }

  test("should declare a coroutine with a throw statement") {
    val rube = coroutine { () =>
      throw {
        val str = "boom"
        new Exception(str)
      }
    }
  }

  test("should invoke another coroutine that ") {
    val boom = coroutine { () => throw new Exception }
    val rube = coroutine { () =>
      boom()
    }
  }
}
