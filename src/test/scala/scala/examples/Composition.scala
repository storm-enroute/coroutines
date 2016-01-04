package scala.examples



import scala.collection._
import scala.coroutines._
import scala.util.Failure



object Composition {
  private val optionElems = coroutine { (opt: Option[Int]) =>
    opt match {
      case Some(x) => yieldval(x)
      case None => // do nothing
    }
  }

  private val optionListElems = coroutine { (xs: List[Option[Int]]) =>
    var curr = xs
    while (curr != Nil) {
      optionElems(curr.head)
      curr = curr.tail
    }
  }

  def main(args: Array[String]) {
  }
}
