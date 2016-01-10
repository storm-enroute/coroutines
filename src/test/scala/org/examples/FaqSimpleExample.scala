package org.examples



import org.coroutines._



object FaqSimpleExample {
  val range = coroutine { (n: Int) =>
    var i = 0
    while (i < n) {
      yieldval(i)
      i += 1
    }
  }

  def extract(c: Int <~> Unit): Seq[Int] = {
    var xs: List[Int] = Nil
    while (c.resume) if (c.hasValue) xs ::= c.value
    xs.reverse
  }

  def main(args: Array[String]) {
    val instance = call(range(10))
    val elems = extract(instance)
    assert(elems == (0 until 10))
  }
}
