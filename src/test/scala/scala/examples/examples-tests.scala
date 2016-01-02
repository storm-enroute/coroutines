package scala.examples



import org.scalatest._
import scala.util.Failure



class ExamplesTest extends FunSuite with Matchers {
  test("identity coroutine") {
    Identity.main(Array())
  }

  test("vowel counts") {
    VowelCounts.main(Array())
  }

  test("datatypes") {
    Datatypes.main(Array())
  }

  test("lifecycle") {
    Lifecycle.main(Array())
  }

  test("exceptions") {
    Exceptions.main(Array())
  }
}
