package org.coroutines.extra



import org.coroutines._
import scala.collection._



class Enumerator[@specialized Y](c: Coroutine.Instance[Y, _]) {
}

object Enumerator {
  def apply[Y](c: Coroutine.Instance[Y, _]) = new Enumerator(c)

  def apply[Y](c: Coroutine._0[Y, _]) = new Enumerator(call(c()))
}