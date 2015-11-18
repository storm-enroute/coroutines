package scala.coroutines



import org.scalatest._



class CoroutineTest extends FunSuite with Matchers {

  test("should be declared") {
    val c = coroutine { (x: Int, y: Int) =>
      val sum = x + y
      yieldval(sum)
      val diff1 = x - y
      yieldval(diff1)
      val diff2 = y - x
      diff2
    }
  }

  test("should have the return type that is the least upper bound") {
    val c = coroutine { (x: Int) =>
      yieldval(List(x))
      List(x.toString)
    }
    val d: Coroutine[List[Any]] = c
  }

}
