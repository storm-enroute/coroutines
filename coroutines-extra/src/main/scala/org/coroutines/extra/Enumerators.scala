package org.coroutines.extra



import org.coroutines._
import scala.collection._



// `Y` is the member type of the enumerator.
trait Enumerator[@specialized Y] {
  /** Apply `f` to each value yielded by the coroutine.
   *
   *  Exceptions seen when in calls to `tryValue` are only thrown if `hasException`.
   *  This is so nothing fails if the coroutine pauses but does not yield a value. For
   *  instance, this avoids exceptions from `yieldto` points.
   *
   *  @param f The function to apply to each value yielded by the coroutine
   */
  def foreach(f: (Y) => Unit)

  /** Apply `f` to each value yielded by the coroutine, and collect the results into
   *  a `immutable.List[Z]`.
   *
   *  Internally, this calls `foreach`.
   *
   *  @param  `f` The function to apply to each value yielded by the coroutine
   *  @return A `List` holding the results of the applications of `f`.
   */
  def map[@specialized Z](f: (Y) => Z): immutable.List[Z]
}