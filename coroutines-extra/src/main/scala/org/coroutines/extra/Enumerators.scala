package org.coroutines.extra



import org.coroutines._
import scala.collection._



class Enumerator[@specialized Y](c: Coroutine.Instance[Y, _]) {
  def hasNext(): Boolean = c.isLive

  def next(): Y = {
     c.pull
     c.value
  }
}

object Enumerator {
  def apply[Y](c: Coroutine.Instance[Y, _]) = new Enumerator(c)

  def apply[Y](c: Coroutine._0[Y, _]) = new Enumerator(call(c()))
}