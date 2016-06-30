package org.coroutines.extra



import org.coroutines._
import scala.collection._



object Enumerators {
  // Map not used directly so that we can avoid a function call
  def toList_0[@specialized Y, R](c: Coroutine._0[Y, R]): immutable.List[Y] = {
    val instance = call(c())
    var result = immutable.Seq.empty[Y]
    instance foreach { element =>
      result = result :+ element
    }
    result.toList
  }

  def toIterator_0[@specialized Y, R](c: Coroutine._0[Y, R]): Iterator[Y] = {
    toList_0(c).toIterator
  }

  def toCoroutine[@specialized Y](i: Iterator[Y]): Coroutine._0[Y, Unit] = {
    coroutine { () =>
      while (i.hasNext) {
        yieldval(i.next)
      }
    }
  }
}
