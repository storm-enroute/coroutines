package org.examples



import org.coroutines._



object Identity {
  val id = coroutine { (x: Int) => x }

  def main(args: Array[String]) {
    val c = call(id(7))
    assert(!c.resume)
    assert(c.result == 7)
  }
}
