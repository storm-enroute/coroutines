package scala.examples



import scala.coroutines._



object Datatypes {
  val whileRange = coroutine { (n: Int) =>
    var i = 0
    while (i < n) {
      yieldval(i)
      i += 1
    }
  }

  val doWhileRange = coroutine { (n: Int) =>
    var i = 0
    do {
      yieldval(i)
      i += 1
    } while (i < n)
  }

  def assertEqualsRange(n: Int, co: Int ~~> (Int, Unit)) {
    val c = call(co(n))
    for (i <- 0 until n) {
      c.resume
      assert(c.value == i)
    }
    assert(!c.resume)
  }

  def main(args: Array[String]) {
    assertEqualsRange(5, whileRange)
    assertEqualsRange(5, doWhileRange)
  }
}
