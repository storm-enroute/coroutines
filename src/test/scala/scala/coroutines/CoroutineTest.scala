package scala.coroutines



import org.scalatest._



class CoroutineTest extends FunSuite with Matchers {
  test("should not yield") {
    val getOk = coroutine { () => "ok" }
    val c = call(getOk())
    assert(c() == "ok")
  }

  test("should yield once") {
    val plusMinus = coroutine { (x: Int) =>
      yieldval(x)
      -x
    }
    val c = call(plusMinus(5))
    assert(c() == 5)
    assert(c() == -5)
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
    assert(c() == 3)
    assert(c() == -1)
    assert(c() == 1)
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
    val c2 = call(xOrY(-2, 7))
    assert(c2() == 7)
    assert(c2() == (()))
  }

  test("should declare body with a coroutine call") {
    val doubleInt = coroutine { (x: Int) => 2 * x }
    val callOther = coroutine { (x: Int) =>
      val y = doubleInt(x)
      y
    }
    val c = call(callOther(5))
    assert(c() == 10)
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
  }

  test("coroutine should contain two assignments at the end of two branches") {
    val c1 = coroutine { (x: Int) => 2 * x }
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
}
