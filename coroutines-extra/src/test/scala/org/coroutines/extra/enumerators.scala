package org.coroutines.extra



import org.coroutines._
import org.scalatest._



class EnumeratorsTest extends FunSuite with Matchers {
  import Enumerators._

  test("simple list creation") {
    val rube = coroutine { () =>
      yieldval(1)
      yieldval(2)
      yieldval(3)
    }
    val list = toList_0(rube)
    assert(list.size == 3)
    assert(list(2) == 3)
  }

  test("simple iterator creation") {
    val rube = coroutine { () =>
      yieldval(1)
      yieldval(2)
      yieldval(3)
    }
    val list = scala.collection.immutable.List(1, 2, 3)
    val iterator = toIterator_0(rube)
    assert(iterator.hasNext)
    assert(iterator.next() == 1)
    assert(iterator.next() == 2)
    assert(iterator.next() == 3)
  }
}
