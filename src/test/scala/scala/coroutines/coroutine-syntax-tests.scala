package scala.coroutines



import org.scalatest._
import scala.util.Failure



class CoroutineSyntaxTest extends FunSuite with Matchers {
  test("Coroutine._0 must be invoked") {
    val rube = coroutine { () =>
      yieldval(5)
      yieldval(-5)
      "ok"
    }
    val co: ~~~>[Int, String] = rube
    val c = call(co())
    assert(c.resume)
    assert(c.value == 5)
    assert(c.resume)
    assert(c.value == -5)
    assert(!c.resume)
    assert(c.result == "ok")
    assert(c.isCompleted)
  }

  test("Coroutine._1 must be invoked") {
    val rube: Coroutine._1[Int, Int, String] = coroutine { (x: Int) =>
      yieldval(x + x)
      yieldval(x - 2 * x)
      "ok" * x
    }

    val co: Int ~~> (Int, String) = rube
    val c = call(co(7))
    assert(c.resume)
    assert(c.value == 14)
    assert(c.resume)
    assert(c.value == -7)
    assert(!c.resume)
    assert(c.result == "ok" * 7)
    assert(c.isCompleted)
  }

  test("Coroutine._1 must be invoked for a tuple argument") {
    val rube = coroutine { (t: (Int, String)) =>
      yieldval(t._1)
      t._2
    }

    val co: (Int, String) ~~> (Int, String) = rube
    val c = call(co((7, "ok")))
    assert(c.resume)
    assert(c.value == 7)
    assert(!c.resume)
    assert(c.result == "ok")
    assert(c.isCompleted)
  }

  test("Coroutine._2 must be invoked") {
    val rube = coroutine { (x: Int, y: Int) =>
      yieldval(x + y)
      yieldval(x - y)
      (x * y).toString
    }

    val co: (Int, Int) ~> (Int, String) = rube
    val c = call(co(7, 4))
    assert(c.resume)
    assert(c.value == 11)
    assert(c.resume)
    assert(c.value == 3)
    assert(!c.resume)
    assert(c.result == "28")
    assert(c.isCompleted)
  }

  test("Coroutine._3 must be invoked") {
    val rube = coroutine { (x: Int, y: Int, z: Int) =>
      yieldval(x)
      yieldval(y)
      z.toString
    }

    val co: (Int, Int, Int) ~> (Int, String) = rube
    val c = call(co(3, 5, 8))
    assert(c.resume)
    assert(c.value == 3)
    assert(c.resume)
    assert(c.value == 5)
    assert(!c.resume)
    assert(c.result == "8")
    assert(c.isCompleted)
  }

  test("Another coroutine must be invoked without syntax sugar") {
    val gimmeFive = coroutine { () => 5 }
    val rube: ~~~>[Nothing, Int] = coroutine { () =>
      gimmeFive()
    }

    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 5)
    assert(c.isCompleted)
  }

  test("Another arity-0 coroutine must be invoked with syntax sugar") {
    val gimmeFive: ~~~>[Nothing, Int] = coroutine { () => 5 }
    val rube = coroutine { () =>
      gimmeFive()
    }

    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 5)
    assert(c.isCompleted)
  }

  test("Another arity-1 coroutine must be invoked with syntax sugar") {
    val neg: Int ~~> (Nothing, Int) = coroutine { (x: Int) => -x }
    val rube = coroutine { () =>
      neg(17)
    }

    val c = call(rube())
    assert(!c.resume)
    assert(c.result == -17)
    assert(c.isCompleted)
  }

  test("Another arity-2 coroutine must be invoked with syntax sugar") {
    val mult: (Int, Int) ~> (Nothing, Int) = coroutine { (x: Int, y: Int) => x * y }
    val rube = coroutine { () =>
      mult(3, 4)
    }

    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 12)
    assert(c.isCompleted)
  }

  test("Another arity-3 coroutine must be invoked with syntax sugar") {
    val mult: (Int, Int, Int) ~> (Nothing, Int) = coroutine {
      (x: Int, y: Int, z: Int) => x * y * z
    }
    val rube = coroutine { () =>
      mult(3, 4, 5)
    }

    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 60)
    assert(c.isCompleted)
  }
}
