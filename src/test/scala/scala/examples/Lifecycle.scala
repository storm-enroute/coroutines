package scala.examples



import scala.coroutines._



object Lifecycle {
  val katamari: Int ~~> (String, Int) = coroutine { (n: Int) =>
    var i = 1
    yieldval("naaaaaa")
    while (i < n) {
      yieldval("na")
      i += 1
    }
    yieldval("Katamari Damacy!")
    i
  }

  def main(args: Array[String]) {
    val c = call(katamari(5))
    assert(c.resume)
  }
}
