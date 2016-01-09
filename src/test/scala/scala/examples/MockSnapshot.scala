package scala.examples



import scala.coroutines._
import scala.util._



object MockSnapshot {
  abstract class TestSuite {
    class Cell {
      var value = false
    }

    class Mock {
      val get = coroutine { () =>
        val cell = new Cell
        yieldval(cell)
        cell.value
      }
    }

    def test[R](c: Cell <~> R): Try[R] = {
      def test[R](c: Cell <~> R): Unit = {
        if (c.resume) {
          val cell = c.value
          cell.value = true
          test(c.snapshot)
          cell.value = false
          test(c)
        }
      }
      test(c)
      c.tryResult
    }
  }

  class MyTestSuite extends TestSuite {
    val myMockCondition = new Mock

    val myAlgorithm = coroutine { (x: Int) =>
      if (myMockCondition.get()) {
        assert(2 * x == x + x)
      } else {
        assert(x * x / x == x)
      }
    }

    assert(test(call(myAlgorithm(5))).isSuccess)

    assert(test(call(myAlgorithm(0))).isFailure)
  }

  def main(args: Array[String]) {
    new MyTestSuite
  }
}
