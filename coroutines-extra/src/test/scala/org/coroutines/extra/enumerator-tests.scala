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

  test("enumerator creation from coroutine instance") {
    val instance = call(rube())
    val enumerator = Enumerator(instance)
  }

  test("enumerator creation from coroutine_0") {
    val enumerator = Enumerator(rube)
  }
}
