package org.examples



import org.coroutines._



object Snapshot {
  val values = coroutine { () =>
    yieldval(1)
    yieldval(2)
    yieldval(3)
  }

  def main(args: Array[String]) {
    val c = call(values())
    assert(c.resume)
    assert(c.value == 1)
    val c2 = c.snapshot
    assert(c.resume)
    assert(c.value == 2)
    assert(c.resume)
    assert(c.value == 3)
    assert(c2.resume)
    assert(c2.value == 2)
    assert(c2.resume)
    assert(c2.value == 3)
  }
}
