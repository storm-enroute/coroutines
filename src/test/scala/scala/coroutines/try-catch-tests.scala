package scala.coroutines



import org.scalatest._
import scala.util.Failure



class TryCatchTest extends FunSuite with Matchers {
  test("coroutine with a try-catch block") {
    val rube = coroutine { () =>
      try {
        throw new Exception
      } catch {
        case e: Exception =>
      }
    }

    val c0 = call(rube())
    assert(c0() == (()))
    assert(c0.isStopped)
  }

  test("coroutine with a try-catch-finally block") {
    val rube = coroutine { () =>
      try {
        throw new Error
      } catch {
        case e: Error =>
      } finally {
        sys.error("done")
      }
    }

    val c0 = call(rube())
    c0.tryGet() match {
      case Failure(re: RuntimeException) => assert(re.getMessage == "done")
      case _ => assert(false)
    }
  }

  test("coroutine with a try-catch-finally and several exception types") {
    var completed = false
    var runtime = false
    var error = false
    val rube = coroutine { (t: Throwable) =>
      try {
        throw t
      } catch {
        case e: RuntimeException =>
          runtime = true
        case e: Error =>
          error = true
      } finally {
        completed = true
      }
    }

    val c0 = call(rube(new Error))
    assert(!runtime)
    assert(!error)
    assert(!completed)
    assert(c0() == (()))
    assert(!runtime)
    assert(error)
    assert(completed)
    assert(c0.isStopped)
  }

  // test("should declare a coroutine with a throw statement") {
  //   val rube = coroutine { () =>
  //     throw {
  //       val str = "boom"
  //       new Exception(str)
  //     }
  //   }
  // }

  // test("should invoke another coroutine that ") {
  //   val boom = coroutine { () => throw new Exception }
  //   val rube = coroutine { () =>
  //     boom()
  //   }
  // }
}
