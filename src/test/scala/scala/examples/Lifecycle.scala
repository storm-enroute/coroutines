package scala.examples



import scala.collection._
import scala.coroutines._
import scala.util.Success



object Lifecycle {
  val katamari: Int ~~> (String, Int) = coroutine { (n: Int) =>
    var i = 1
    yieldval("naaaa")
    while (i < n) {
      yieldval("na")
      i += 1
    }
    yieldval("Katamari Damacy!")
    i + 2
  }

  def main(args: Array[String]) {
    val c = call(katamari(9))
    assert(c.resume)
    assert(c.hasValue)
    assert(c.value == "naaaa")
    for (i <- 1 until 9) {
      assert(c.resume)
      assert(c.getValue == Some("na"))
    }
    assert(c.resume)
    assert(c.tryValue == Success("Katamari Damacy!"))
    assert(!c.resume)
    assert(c.getValue == None)
    assert(c.result == 11)
    assert(c.isCompleted)
    assert(!c.isLive)

    val theme = "naaaa na na na na na na na na Katamari Damacy!"
    assert(drain(call(katamari(9))) == theme)
  }

  def drain(f: String <~> Int): String = {
    val buffer = mutable.Buffer[String]()
    while (f.resume) buffer += f.value
    buffer.mkString(" ")
  }
}
