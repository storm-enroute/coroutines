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

  test("should lub yieldvals and returns") {
    val c = coroutine { (x: Int) =>
      yieldval(List(x))
      List(x.toString)
    }
    val d: Coroutine.Definition[List[Any]] = c
  }

  test("should lub yieldtos and returns") {
    val f: Coroutine[List[String]] = null
    val c = coroutine { (x: Int) =>
      yieldto(f)
      Vector(x)
    }
    val d: Coroutine.Definition[Seq[Any]] = null 
  }

  test("should declare body with if statement") {
    val c = coroutine { (x: Int, y: Int) =>
      if (x > 0) {
        yieldval(x)
      } else {
        yieldval(y)
      }
      x
    }
  }

  test("should declare body with a coroutine call") {
    val c1 = coroutine { (x: Int) => x }
    val c2 = coroutine { (x: Int) =>
      val y = c1(x)
      y
    }
  }

  test("should declare a variable in a nested scope") {
    val c = coroutine { (x: Int, y: Int) =>
      if (x > 0) {
        val z = -x
        yieldval(z)
        yieldval(-z)
      } else {
        yieldval(y)
      }
      x
    }
  }

}
