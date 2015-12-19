package scala.coroutines



import org.scalatest._
import scala.util.Failure



class ThreeAddressFormTransformationTest extends FunSuite with Matchers {
  test("if state ments with applications") {
    val rube = coroutine { () =>
      if (0 < { math.abs(-1); math.max(1, 2) }) 2 else 1
    }
    val c = call(rube())
    assert(c() == 2)
  }

  test("if statements with applications and yield") {
    val rube = coroutine { () =>
      val x = if (0 < { math.abs(-1); math.max(1, 2) }) 2 else 1
      yieldval(x)
      -x
    }
    val c = call(rube())
    assert(c() == 2)
    assert(c() == -2)
    assert(c.isStopped)
  }

  test("if statements with selections") {
    val rube = coroutine { () =>
      if (0 < { math.abs(math.Pi) }) 2 else 1
    }
    val c = call(rube())
    assert(c() == 2)
  }

  test("if statements with selections and yield") {
    val rube = coroutine { () =>
      val x = if (0 < { math.abs(math.Pi) }) 2 else 1
      yieldval(x)
      -x
    }
    val c = call(rube())
    assert(c() == 2)
    assert(c() == -2)
    assert(c.isStopped)
  }

  test("if statements with updates") {
    val rube = coroutine { () =>
      val xs = new Array[Int](2)
      if (0 < { xs(0) = 1; xs(0) }) 2 else 1
    }
    val c = call(rube())
    assert(c() == 2)
  }

  test("if statements with block in tuple") {
    val rube = coroutine { () =>
      if (0 < ({ math.abs(1); math.abs(3) + 2 }, 2)._1) 2 else 1
    }
    val c = call(rube())
    assert(c() == 2)
  }

  test("if statement with another if statement in condition") {
    val rube = coroutine { () =>
      if (0 < (if (math.abs(-1) > 5) 1 else 2)) 2 else 1
    }
    val c = call(rube())
    assert(c() == 2)
  }

  test("value declaration should be the last statement") {
    val unit = coroutine { () =>
     val t = (2, 3)
     val (y, z) = t
    }

    val c = call(unit())
    assert(c() == (()))
    assert(!c.isAlive)
  }

  test("coroutine should be callable outside value declaration") {
    var y = 0
    val setY = coroutine { (x: Int) => y = x }
    val setTo5 = coroutine { () =>
      setY(5)
    }
    val c = call(setTo5())
    c()
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
    c()
    assert(y == 5)
    c()
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
    for (i <- 0 to 10) assert(c1() == i)
    assert(c1.isStopped)
    val c2 = call(rube(20))
    assert(c2() == 0)
    assert(c2.isStopped)
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
    for (i <- 0 to 10; if i % 2 == 0) assert(c1() == i)
    assert(c1.isStopped)
    val c2 = call(rube(20))
    assert(c2() == 0)
    assert(c2.isStopped)
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
    for (i <- 1 until 10) assert(c1() == i)
    assert(c1() == 117)
    assert(c1.isStopped)
    val c2 =  call(rube(-10))
    assert(c2() == 1)
    assert(c2() == 117)
    assert(c2.isStopped)
  }

  test("yield absolute and original value") {
    val rube = coroutine { (x: Int) =>
      yieldval(math.abs(x))
      x
    }

    val c = call(rube(-5))
    assert(c() == 5)
    assert(c() == -5)
    assert(c.isStopped)
  }

  test("short-circuiting should work for and") {
    var state = "untouched"
    val rube = coroutine { (x: Int) =>
      if (x < 0 && { state = "touched"; true }) x
      else -x
    }
    
    val c0 = call(rube(5))
    assert(c0() == -5)
    assert(c0.isStopped)
    assert(state == "untouched")

    val c1 = call(rube(-5))
    assert(c1() == -5)
    assert(c1.isStopped)
    assert(state == "touched")
  }

  test("short-circuiting should work for or") {
    var state = "untouched"
    val rube = coroutine { (x: Int) =>
      if (x > 0 || { state = "touched"; false }) x
      else -x
    }
    
    val c0 = call(rube(5))
    assert(c0() == 5)
    assert(c0.isStopped)
    assert(state == "untouched")

    val c1 = call(rube(-5))
    assert(c1() == 5)
    assert(c1.isStopped)
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
    assert(c0() == 0)
    assert(c0() == 1)
    assert(c0() == 2)
    assert(c0() == 3)
    assert(c0() == 4)
    assert(c0() == 5)
    assert(c0.isStopped)

    val c1 = call(rube(0))
    assert(c1() == 0)
    assert(c1() == 1)
    assert(c1.isStopped)
  }
}
