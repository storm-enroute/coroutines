package scala.examples



import scala.collection._
import scala.coroutines._
import scala.util.Failure



object CompositionCall {
  private val optionElems = coroutine { (opt: Option[Int]) =>
    opt match {
      case Some(x) => yieldval(x)
      case None => // do nothing
    }
  }

  private val optionListElems = coroutine { (xs: List[Option[Int]]) =>
    var curr = xs
    while (curr != Nil) {
      val c = call(optionElems(curr.head))
      while (c.resume) yieldval(c.value)
      curr = curr.tail
    }
  }

  def main(args: Array[String]) {
    val xs = Some(1) :: None :: Some(3) :: Nil
    val c = call(optionListElems(xs))
    assert(c.resume)
    assert(c.value == 1)
    assert(c.resume)
    assert(c.value == 3)
    assert(!c.resume)
  }
}
