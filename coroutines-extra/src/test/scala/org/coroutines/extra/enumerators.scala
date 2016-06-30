package org.coroutines.extra



import org.coroutines._
import org.scalatest._
import scala.collection._



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

  test("list creation from inner coroutine") {
    val outerRube = coroutine { () =>
      val innerRube = coroutine { () =>
        yieldval(1)
        yieldval(2)
      }
      val innerCall = call(innerRube())
      yieldto(innerCall)
      yieldval(3)
    }
    val list = toList_0(outerRube)
    assert(list.sum == 3)
  }

  test("iterator creation from inner coroutine") {
    val outerRube = coroutine { () =>
      val innerRube = coroutine { () =>
        yieldval(1)
        yieldval(2)
      }
      val innerCall = call(innerRube())
      yieldto(innerCall)
      yieldval(3)
    }
    assert(toIterator_0(outerRube).sum == 3)
  }

  test("yield to iterator") {
    val iterator = Iterator(1, 2, 3)
    val rube = coroutine { () =>
      val c = toCoroutine(iterator)
      yieldto(call(c()))
      yieldval(4)
      yieldval(5)
    }
    assert(toList_0(rube).sum == 9)
  }

  test("iterator to coroutine to iterator must yield normally") {
    val iterator = Iterator(1, 2, 3)
    val rube = toCoroutine(iterator)
    assert(toIterator_0(rube).sum == 6)
  }

  test("iterator to coroutine to list must yield normally") {
    val iterator = Iterator(1, 2, 3)
    val rube = toCoroutine(iterator)
    assert(toList_0(rube).sum == 6)
  }
}
