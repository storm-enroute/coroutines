package scala.examples



import scala.coroutines._



object VowelCounts {
  val vowelcounts = coroutine { (s: String) =>
    yieldval(s.count(_ == 'a'))
    yieldval(s.count(_ == 'e'))
    yieldval(s.count(_ == 'i'))
    yieldval(s.count(_ == 'o'))
    s.count(_ == 'u')
  }

  def main(args: Array[String]) {
    val c = call(vowelcounts("this the season to be jolie"))
    assert(c() == 1)
    assert(c() == 4)
    assert(c() == 2)
    assert(c() == 3)
    assert(c() == 0)
  }
}
