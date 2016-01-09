package scala.examples



import scala.collection._
import scala.coroutines._
import scala.util.Random



object ControlTransfer {
  val check: ~~~>[Boolean, Unit] = coroutine { () =>
    yieldval(true)
    yieldval(false)
  }
  val checker = call(check())

  val random: ~~~>[Double, Unit] = coroutine { () =>
    yieldval(Random.nextDouble())
    yieldto(checker)
    yieldval(Random.nextDouble())
  }

  def main(args: Array[String]) {
    val r0 = call(random())
    assert(r0.resume)
    assert(r0.hasValue)
    assert(r0.resume)
    assert(!r0.hasValue)
    assert(r0.resume)
    assert(r0.hasValue)
    assert(!r0.resume)
    assert(!r0.hasValue)

    val r1 = call(random())
    val values = mutable.Buffer[Double]()
    while (r1.resume) if (r1.hasValue) values += r1.value
    assert(values.length == 2)
  }
}
