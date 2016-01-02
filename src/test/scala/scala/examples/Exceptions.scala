package scala.examples



import scala.collection._
import scala.coroutines._
import scala.util.Failure



object Exceptions {
  case class TestException() extends Throwable

  val kaboom = coroutine { (x: Int) =>
    yieldval(x)
    try {
      sys.error("will be caught")
    } catch {
      case e: RuntimeException => yieldval("oops")
    }
    throw TestException()
  }

  def main(args: Array[String]) {
    val c = call(kaboom(5))
    assert(c.resume)
    assert(c.value == 5)
    assert(c.resume)
    assert(c.value == "oops")
    assert(!c.resume)
    assert(c.tryResult == Failure(TestException()))
  }
}
