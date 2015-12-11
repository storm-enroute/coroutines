package scala.coroutines.common



import scala.collection._



object Cache {
  def cached[T, S](f: T => S): T => S = {
    val cache = mutable.Map[T, S]()
    t => {
      cache.get(t) match {
        case Some(s) => s
        case None =>
          val s = f(t)
          cache(t) = s
          s
      }
    }
  }

  def cached[T1, T2, S](f: (T1, T2) => S): (T1, T2) => S = {
    val cache = mutable.Map[(T1, T2), S]()
    (t1, t2) => {
      cache.get((t1, t2)) match {
        case Some(s) => s
        case None =>
          val s = f(t1, t2)
          cache((t1, t2)) = s
          s
      }
    }
  }
}
