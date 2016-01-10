package org.coroutines.common



import scala.collection._



object Cache {
  class _1[T, S](val function: T => S) {
    val cache = mutable.Map[T, S]()
    def apply(t: T): S = cache.get(t) match {
      case Some(s) => s
      case None =>
        val s = function(t)
        cache(t) = s
        s
    }
  }

  def cached[T, S](f: T => S): _1[T, S] = new _1(f)

  class _2[T1, T2, S](val function: (T1, T2) => S) {
    val cache = mutable.Map[(T1, T2), S]()
    def apply(t1: T1, t2: T2): S = cache.get((t1, t2)) match {
      case Some(s) => s
      case None =>
        val s = function(t1, t2)
        cache((t1, t2)) = s
        s
    }
  }

  def cached[T1, T2, S](f: (T1, T2) => S): _2[T1, T2, S] = new _2(f)

  class _3[T1, T2, T3, S](val function: (T1, T2, T3) => S) {
    val cache = mutable.Map[(T1, T2, T3), S]()
    def apply(t1: T1, t2: T2, t3: T3): S = cache.get((t1, t2, t3)) match {
      case Some(s) => s
      case None =>
        val s = function(t1, t2, t3)
        cache((t1, t2, t3)) = s
        s
    }
  }

  def cached[T1, T2, T3, S](f: (T1, T2, T3) => S): _3[T1, T2, T3, S] = new _3(f)
}
