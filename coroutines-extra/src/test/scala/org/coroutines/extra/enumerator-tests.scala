package org.coroutines.extra



import org.coroutines._
import org.scalatest._
import scala.collection._



class EnumeratorsTest extends FunSuite with Matchers {
  val rube = coroutine { () =>
    yieldval(1)
    yieldval(2)
    yieldval(3)
  }

  // Asserts that `apply` takes a `snapshot` of the instance.
  test("enumerator creation from coroutine instance") {
    val instance = call(rube())

    val enumerator1 = Enumerator(instance)
    assert(enumerator1.hasNext())
    assert(enumerator1.next == 1)
    assert(enumerator1.next == 2)
    assert(enumerator1.next == 3)
    assert(!enumerator1.hasNext)

    val enumerator2 = Enumerator(instance)
    assert(enumerator2.hasNext())
    assert(enumerator2.next == 1)
    assert(enumerator2.next == 2)
    assert(enumerator2.next == 3)
    assert(!enumerator2.hasNext)
  }

  /** Asserts that more than one `Enumerator` can be created from the same
   *  `Coroutine._0`.
   */
  test("enumerator creation from coroutine_0") {
    val enumerator1 = Enumerator(rube)
    assert(enumerator1.hasNext())
    assert(enumerator1.next == 1)
    assert(enumerator1.next == 2)
    assert(enumerator1.next == 3)
    assert(!enumerator1.hasNext)

    val enumerator2 = Enumerator(rube)
    assert(enumerator2.hasNext())
    assert(enumerator2.next == 1)
    assert(enumerator2.next == 2)
    assert(enumerator2.next == 3)
    assert(!enumerator2.hasNext)
  }

  test("enumerator creation from code block") {
    val enumerator = Enumerator {
      var i = 0
      while (i < 5) {
        yieldval(i)
        i += 1
      }
    }
    assert(enumerator.hasNext())
    for (i <- 0 until 5) {
      assert(enumerator.next == i)
    }
    assert(!enumerator.hasNext)
  }

  test("enumerator should ignore return value of coroutine") {
    val rubeWithReturn = coroutine { () =>
      yieldval(1)
      yieldval(2)
      yieldval(3)
      "foo"
    }
    val enumerator = Enumerator(rubeWithReturn)
    assert(enumerator.hasNext())
    assert(enumerator.next == 1)
    assert(enumerator.next == 2)
    assert(enumerator.next == 3)
    assert(!enumerator.hasNext)
  }
}
