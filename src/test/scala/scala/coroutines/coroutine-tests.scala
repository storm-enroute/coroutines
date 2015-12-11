package scala.coroutines



import org.scalatest._
import scala.util.Failure



class CoroutineTest extends FunSuite with Matchers {
  test("should not yield") {
    val getOk = coroutine { () => "ok" }
    val c = call(getOk())
    assert(c.isAlive)
    assert(c() == "ok")
    assert(!c.isAlive)
  }

  test("should throw when not alive") {
    val gimmeFive = coroutine { () => 5 }
    val c = call(gimmeFive())
    assert(c.isAlive)
    assert(c() == 5)
    assert(!c.isAlive)
    intercept[CoroutineStoppedException] {
      c()
    }
    assert(c.get() == None)
    assert(c.tryGet() == Failure(CoroutineStoppedException()))
  }

  test("should yield once") {
    val plusMinus = coroutine { (x: Int) =>
       yieldval(x)
      -x
    }
    val c = call(plusMinus(5))
    assert(c.isAlive)
    assert(c() == 5)
    assert(c.isAlive)
    assert(c() == -5)
    assert(!c.isAlive)
  }

  test("should yield several times") {
    val sumAndDiffs = coroutine { (x: Int, y: Int) =>
      val sum = x + y
      yieldval(sum)
      val diff1 = x - y
      yieldval(diff1)
      val diff2 = y - x
      diff2
    }
    val c = call(sumAndDiffs(1, 2))
    assert(c.isAlive)
    assert(c() == 3)
    assert(c.isAlive)
    assert(c() == -1)
    assert(c.isAlive)
    assert(c() == 1)
    assert(!c.isAlive)
  }

  test("should lub yieldvals and returns") {
    val lists = coroutine { (x: Int) =>
      yieldval(List(x))
      List(x.toString)
    }
    val anotherLists: Coroutine.Blueprint[List[Any]] = lists
    val c = call(lists(5))
    assert(c() == List(5))
    assert(c() == List("5"))
  }

  test("should lub yieldtos and returns") {
    val wrapString = coroutine { (x: String) =>
      List(x.toString)
    }
    val f: Coroutine[List[String]] = call(wrapString("ok"))
    val wrapInt = coroutine { (x: Int) =>
      yieldto(f)
      Vector(x)
    }
    val c = call(wrapInt(7))
    assert(c() == List("ok"))
    assert(c() == Vector(7))
  }

  test("should declare body with if statement") {
    val xOrY = coroutine { (x: Int, y: Int) =>
      if (x > 0) {
        yieldval(x)
      } else {
        yieldval(y)
      }
    }
    val c1 = call(xOrY(5, 2))
    assert(c1() == 5)
    assert(c1() == (()))
    assert(c1.isStopped)
    val c2 = call(xOrY(-2, 7))
    assert(c2() == 7)
    assert(c2() == (()))
    assert(c1.isStopped)
  }

  test("should declare body with a coroutine call") {
    val doubleInt = coroutine { (x: Int) => 2 * x }
    val callOther = coroutine { (x: Int) =>
      val y = doubleInt(x)
      y
    }
    val c = call(callOther(5))
    assert(c() == 10)
    assert(!c.isAlive)
  }

  test("should declare a value in a nested scope") {
    val someValues = coroutine { (x: Int, y: Int) =>
      if (x > 0) {
        val z = -x
        yieldval(z)
        yieldval(-z)
      } else {
        yieldval(y)
      }
      x
    }
    val c1 = call(someValues(5, 7))
    assert(c1() == -5)
    assert(c1() == 5)
    assert(c1() == 5)
    val c2 = call(someValues(-5, 7))
    assert(c2() == 7)
    assert(c2() == -5)
  }

  test("should declare a variable in a nested scope") {
    val someValues = coroutine { (x: Int, y: Int) =>
      if (x > 0) {
        var z = -x
        yieldval(z)
        z = -z
        yieldval(z)
      } else {
        yieldval(x)
      }
      y
    }
    val c1 = call(someValues(6, 11))
    assert(c1() == -6)
    assert(c1() == 6)
    assert(c1() == 11)
    val c2 = call(someValues(-6, 11))
    assert(c2() == -6)
    assert(c2() == 11)
  }

  test("coroutine should be called") {
    val emitTwice = coroutine { (x: Int) =>
      yieldval(x)
      x
    }
    val c = call(emitTwice(7))
    assert(c() == 7)
    assert(c() == 7)
  }

  test("coroutine should contain an if statement and no yields") {
    val abs = coroutine { (x: Int) =>
      if (x > 0) x
      else -x
    }
    val c1 = call(abs(-5))
    assert(c1() == 5)
    val c2 = call(abs(5))
    assert(c2() == 5)
  }

  test("coroutine should contain two applications at the end of two branches") {
    val c1 = coroutine { (x: Int) => x }
    val c2 = coroutine { (x: Int) =>
      if (x > 0) {
        val y = c1(x)
      } else {
        val z = c1(-x)
      }
      x
    }
    val c = call(c2(5))
    assert(c() == 5)
    assert(c.isStopped)
  }

  test("coroutine should contain two assignments at the end of two branches") {
    val c1 = coroutine { (n: Int) => 2 * n }
    val c2 = coroutine { (x: Int) =>
      var y = 0
      if (x > 0) {
        val z = c1(x)
        y = z
      } else {
        val z = c1(-x)
        y = z
      }
      y
    }
    val c = call(c2(5))
    assert(c() == 10)
  }

  test("coroutine should have an integer argument and a string local variable") {
    val stringify = coroutine { (x: Int) =>
      val s = x.toString
      s
    }
    val c = call(stringify(11))
    assert(c() == "11")
  }

  test("coroutine should assign") {
    val assign = coroutine { (x: Int) =>
      var y = 0
      y = x + 1
      y
    }
    val c = call(assign(5))
    assert(c() == 6)
  }

  test("coroutine should contain a while loop") {
    val number = coroutine { () =>
      var i = 0
      while (i < 10) {
        i += 1
      }
      i
    }
    val c = call(number())
    assert(c() == 10)
  }

  test("coroutine should contains a while loop with a yieldval") {
    val numbers = coroutine { () =>
      var i = 0
      while (i < 10) {
        yieldval(i)
        i += 1
      }
      i
    }
    val c = call(numbers())
    for (i <- 0 to 10) assert(c() == i)
    assert(c.isStopped)
  }

  test("coroutine should correctly skip the while loop") {
    val earlyFinish = coroutine { (x: Int) =>
      var i = 1
      while (i < x) {
        yieldval(i) 
        i += 1
      }
      i
    }
    val c = call(earlyFinish(0))
    assert(c() == 1)
    assert(c.isStopped)
  }

  test("coroutine should have a nested if statement") {
    val numbers = coroutine { () =>
      var z = 1
      var i = 1
      while (i < 5) {
        if (z > 0) {
          yieldval(z * i)
          z = -1
        } else {
          yieldval(z * i)
          z = 1
          i += 1
        }
      }
    }
    val c = call(numbers())
    for (i <- 1 until 5) {
      assert(c() == i)
      assert(c() == -i)
    }
  }

  test("an anonymous coroutine should be applied") {
    coroutine { (x: Int) => x }
  }

  test("if statement should be properly regenerated") {
    val addOne = coroutine { (x: Int) =>
      if (x > 0) {
        x
      } else {
        -x
      }
      x + 1
    }
    val c1 = call(addOne(1))
    assert(c1() == 2)
    val c2 = call(addOne(-1))
    assert(c2() == 0)
  }

  test("if statement with unit last statement should be properly generated") {
    val addOne = coroutine { () =>
      var x = 5
      if (0 < x) {
        x = 2
        ()
      } else {
        x = 1
        ()
      }
      x
    }
    val c = call(addOne())
    assert(c() == 2)
  }

  test("coroutine should yield every second element") {
    val rube = coroutine { (x: Int) =>
      var i = 0
      while (i < x) {
        if (i % 2 == 0) yieldval(i)
        i += 1
      }
      i
    }
  }

  test("coroutine should yield x, 117, and -x") {
    val rube = coroutine { (x: Int) =>
      var z = x
      yieldval(z)
      yieldval(117)
      -z
    }
    val c = call(rube(7))
    assert(c() == 7)
    assert(c() == 117)
    assert(c() == -7)
    assert(c.isStopped)
  }
}


class ToaTransformationTest extends FunSuite with Matchers {
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
    val c2 = call(rube(-10))
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
}
