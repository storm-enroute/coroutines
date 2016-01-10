package org.examples



import org.scalatest._
import scala.util.Failure



class ExamplesTest extends FunSuite with Matchers {
  test("identity coroutine") {
    Identity.main(Array())
  }

  test("vowel counts") {
    VowelCounts.main(Array())
  }

  test("datatypes") {
    Datatypes.main(Array())
  }

  test("lifecycle") {
    Lifecycle.main(Array())
  }

  test("exceptions") {
    Exceptions.main(Array())
  }

  test("composition") {
    Composition.main(Array())
  }

  test("composition call") {
    CompositionCall.main(Array())
  }

  test("faq simple example") {
    FaqSimpleExample.main(Array())
  }

  test("control transfer") {
    ControlTransfer.main(Array())
  }

  test("snapshot") {
    Snapshot.main(Array())
  }

  test("mock snapshot") {
    MockSnapshot.main(Array())
  }
}
