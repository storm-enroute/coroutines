package scala.examples



import scala.coroutines._



object VowelCounts {
  val vowelcounts = coroutine { (s: String) =>
    yieldval(s.count(_ == 'a'))
    yieldval(s.count(_ == 'e'))
    yieldval(s.count(_ == 'i'))
    yieldval(s.count(_ == 'o'))
    yieldval(s.count(_ == 'u'))
  }

  def main(args: Array[String]) {
    val c = call(vowelcounts("this the season to be jolie"))
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 4)
    assert(c.resume)
    assert(c.value == 2)
    assert(c.resume)
    assert(c.value == 3)
    assert(c.resume)
    assert(c.value == 0)
  }
}
