package scala.coroutines



import org.scalatest._
import scala.util.Failure



class TryCatchTest extends FunSuite with Matchers {
  test("should declare a coroutine with a try-catch block") {
    val rube = coroutine { () =>
      try {
        throw new Exception
      } catch {
        case e: Exception =>
      }
    }
  }
}
