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
    val anotherLists: Coroutine.Definition[List[Any]] = lists
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

  // test("should declare a value in a nested scope") {
  //   val c = coroutine { (x: Int, y: Int) =>
  //     if (x > 0) {
  //       val z = -x
  //       yieldval(z)
  //       yieldval(-z)
  //     } else {
  //       yieldval(y)
  //     }
  //     x
  //   }
  // }

  // test("should declare a variable in a nested scope") {
  //   val c = coroutine { (x: Int, y: Int) =>
  //     if (x > 0) {
  //       var z = -x
  //       yieldval(z)
  //       z = -z
  //       yieldval(z)
  //     } else {
  //       yieldval(x)
  //     }
  //     y
  //   }
  // }

  // test("coroutine should be called") {
  //   val emitTwice = coroutine { (x: Int) =>
  //     yieldval(x)
  //     x
  //   }
  //   val c = call(emitTwice(1))
  //   c()
  //   c()
  // }

  // test("coroutine should contain an if statement and no yields") {
  //   val c = coroutine { (x: Int) =>
  //     if (x > 0) x
  //     else -x
  //   }
  // }

  // test("coroutine should contain two applications at the end of two branches") {
  //   val c1 = coroutine { (x: Int) => x }
  //   val c2 = coroutine { (x: Int) =>
  //     if (x > 0) {
  //       val y = c1(x)
  //     } else {
  //       val z = c1(-x)
  //     }
  //     x
  //   }
  // }

  // test("coroutine should have an integer argument and a string local variable") {
  //   val c = coroutine { (x: Int) =>
  //     val s = x.toString
  //     s
  //   }
  // }

  // test("coroutine should assign") {
  //   val c1 = coroutine { (x: Int) => x }
  //   val c2 = coroutine { (x: Int) =>
  //     var y = 0
  //     y = 1
  //     y
  //   }
  // }

  // test("coroutine should contain a while loop") {
  //   val c = coroutine { () =>
  //     var i = 0
  //     while (i < 10) {
  //       i += 1
  //     }
  //     i
  //   }
  // }

  // test("coroutine should contains a while loop with a yieldval") {
  //   val c = coroutine { () =>
  //     var i = 0
  //     while (i < 10) {
  //       yieldval(i)
  //       i += 1
  //     }
  //     i
  //   }
  // }
}
