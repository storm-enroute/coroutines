package org.coroutines.extra



import org.coroutines._
import scala.collection._



// Ignores the return value of the coroutine for specialization and cleanliness.
// Uses a coroutine instance over a coroutine so that both the constructor is more
// general and so that an enumerator can be built from an in-progress coroutine.
class Enumerator[@specialized Y](instance: Coroutine.Instance[Y, _]) {
  private var _hasNext = instance.pull

  def hasNext(): Boolean = _hasNext

  /** Returns the next value the enumerator should return.
   *
   *  Also advances the enumerator to the next return point. This might be an
   *  abstraction leak, since it is internally different from how iterators and
   *  streams operate. However, this approach meets our goal of ignoring the
   *  coroutine's return value. In addition, it does not reference any internal
   *  coroutine methods or members, which would not be ideal for code quality.
   */
  def next(): Y = {
    val result = instance.value
    _hasNext = instance.pull
    result
  }
}

object Enumerator {
  def apply[Y](c: Coroutine.Instance[Y, _]) = new Enumerator(c)

  def apply[Y](c: Coroutine._0[Y, _]) = new Enumerator(call(c()))
}