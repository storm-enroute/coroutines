package scala.coroutines



import org.scalatest._



class CoroutineTest extends FunSuite with Matchers {

  test("should be declared") {
    val c = coroutine {
      "ok"
    }
  }

}
