package org.coroutines



import org.scalatest._
import scala.util.Failure



class ASTCanonicalizationTest extends FunSuite with Matchers {
  test("if statements with applications") {
    val rube = coroutine { () =>
      if (0 < { math.abs(-1); math.max(1, 2) }) 2 else 1
    }
    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 2)
  }

  test("if statements with applications and yield") {
    val rube = coroutine { () =>
      val x = if (0 < { math.abs(-1); math.max(1, 2) }) 2 else 1
      yieldval(x)
      -x
    }
    val c = call(rube())
    assert(c.resume)
    assert(c.value == 2)
    assert(!c.resume)
    assert(c.result == -2)
    assert(c.isCompleted)
  }

  test("if statements with selections") {
    val rube = coroutine { () =>
      if (0 < { math.abs(math.Pi) }) 2 else 1
    }
    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 2)
  }

  test("if statements with selections and yield") {
    val rube = coroutine { () =>
      val x = if (0 < { math.abs(math.Pi) }) 2 else 1
      yieldval(x)
      -x
    }
    val c = call(rube())
    assert(c.resume)
    assert(c.value == 2)
    assert(!c.resume)
    assert(c.result == -2)
    assert(c.isCompleted)
  }

  test("if statements with updates") {
    val rube = coroutine { () =>
      val xs = new Array[Int](2)
      if (0 < { xs(0) = 1; xs(0) }) 2 else 1
    }
    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 2)
  }

  test("if statements with block in tuple") {
    val rube = coroutine { () =>
      if (0 < ({ math.abs(1); math.abs(3) + 2 }, 2)._1) 2 else 1
    }
    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 2)
  }

  test("if statement with another if statement in condition") {
    val rube = coroutine { () =>
      if (0 < (if (math.abs(-1) > 5) 1 else 2)) 2 else 1
    }
    val c = call(rube())
    assert(!c.resume)
    assert(c.result == 2)
  }

  test("value declaration should be the last statement") {
    val unit = coroutine { () =>
     val t = (2, 3)
     val (y, z) = t
    }

    val c = call(unit())
    assert(!c.resume)
    assert(!c.isLive)
    c.result
    assert(!c.hasException)
  }

  test("coroutine should be callable outside value declaration") {
    var y = 0
    val setY = coroutine { (x: Int) => y = x }
    val setTo5 = coroutine { () =>
      setY(5)
    }
    val c = call(setTo5())
    assert(!c.resume)
    assert(y == 5)
  }

  test("coroutine should be callable outside value declaration and yield") {
    var y = 0
    val setY = coroutine { (x: Int) => y = x }
    val setTo5 = coroutine { () =>
      yieldval(setY(5))
      setY(-5)
    }
    val c = call(setTo5())
    assert(c.resume)
    assert(y == 5)
    assert(!c.resume)
    assert(y == -5)
  }

  test("coroutine should yield in while loop with complex condition") {
    val rube = coroutine { (x: Int) =>
      var i = 0
      while (i < x && x < math.abs(-15)) {
        yieldval(i)
        i += 1
      }
      i
    }
    val c1 = call(rube(10))
    for (i <- 0 until 10) {
      assert(c1.resume)
      assert(c1.value == i)
    }
    assert(!c1.resume)
    assert(c1.result == 10)
    assert(c1.isCompleted)
    val c2 = call(rube(20))
    assert(!c2.resume)
    assert(c2.result == 0)
    assert(c2.isCompleted)
  }

  test("coroutine should yield every second element or just zero") {
    val rube = coroutine { (x: Int) =>
      var i = 0
      while (i < x && x < math.abs(-15)) {
        if (i % 2 == 0) yieldval(i)
        i += 1
      }
      i
    }

    val c1 = call(rube(10))
    for (i <- 0 until 10; if i % 2 == 0) {
      assert(c1.resume)
      assert(c1.value == i)
    }
    assert(!c1.resume)
    assert(c1.result == 10)
    assert(c1.isCompleted)
    val c2 = call(rube(20))
    assert(!c2.resume)
    assert(c2.result == 0)
    assert(c2.isCompleted)
  }

  test("coroutine should yield 1 or yield 10 elements, and then 117") {
    val rube = coroutine { (x: Int) =>
      var i = 1
      if (x > math.abs(0)) {
        while (i < x) {
          yieldval(i)
          i += 1
        }
      } else {
        yieldval(i)
      }
      117
    }

    val c1 = call(rube(10))
    for (i <- 1 until 10) {
      assert(c1.resume)
      assert(c1.value == i)
    }
    assert(!c1.resume)
    assert(c1.result == 117)
    assert(c1.isCompleted)
    val c2 = call(rube(-10))
    assert(c2.resume)
    assert(c2.value == 1)
    assert(!c2.resume)
    assert(c2.result == 117)
    assert(c2.isCompleted)
  }

  test("yield absolute and original value") {
    val rube = coroutine { (x: Int) =>
      yieldval(math.abs(x))
      x
    }

    val c = call(rube(-5))
    assert(c.resume)
    assert(c.value == 5)
    assert(!c.resume)
    assert(c.result == -5)
    assert(c.isCompleted)
  }

  test("short-circuiting should work for and") {
    var state = "untouched"
    val rube = coroutine { (x: Int) =>
      if (x < 0 && { state = "touched"; true }) x
      else -x
    }
    
    val c0 = call(rube(5))
    assert(!c0.resume)
    assert(c0.result == -5)
    assert(c0.isCompleted)
    assert(state == "untouched")

    val c1 = call(rube(-5))
    assert(!c1.resume)
    assert(c1.result == -5)
    assert(c1.isCompleted)
    assert(state == "touched")
  }

  test("short-circuiting should work for or") {
    var state = "untouched"
    val rube = coroutine { (x: Int) =>
      if (x > 0 || { state = "touched"; false }) x
      else -x
    }
    
    val c0 = call(rube(5))
    assert(!c0.resume)
    assert(c0.result == 5)
    assert(c0.isCompleted)
    assert(state == "untouched")

    val c1 = call(rube(-5))
    assert(!c1.resume)
    assert(c1.result == 5)
    assert(c1.isCompleted)
    assert(state == "touched")
  }

  test("do-while should be simplified into a while loop") {
    val rube = coroutine { (x: Int) =>
      var i = 0
      do {
        yieldval(i)

        i += 1
      } while (i < x)
      i
    }

    val c0 = call(rube(5))
    assert(c0.resume)
    assert(c0.value == 0)
    assert(c0.resume)
    assert(c0.value == 1)
    assert(c0.resume)
    assert(c0.value == 2)
    assert(c0.resume)
    assert(c0.value == 3)
    assert(c0.resume)
    assert(c0.value == 4)
    assert(!c0.resume)
    assert(c0.result == 5)
    assert(c0.isCompleted)

    val c1 = call(rube(0))
    assert(c1.resume)
    assert(c1.value == 0)
    assert(!c1.resume)
    assert(c1.result == 1)
    assert(c1.isCompleted)
  }
}
