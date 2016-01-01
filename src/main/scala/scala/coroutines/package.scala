package scala



import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



package object coroutines {

  val COROUTINE_DIRECT_APPLY_ERROR_MESSAGE =
    "Coroutines can only be invoked directly from within other coroutines. " +
    "Use `call(<coroutine>(<arg0>, ..., <argN>))` instead if you want to " +
    "start a new coroutine."

  case class CoroutineStoppedException() extends Exception

  def yieldval[T](x: T): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def yieldto[T](f: Coroutine.Inst[T]): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def call[T](f: T): Coroutine.Inst[T] = macro Coroutine.call[T]

  def coroutine[T](f: Any): Any = macro Coroutine.synthesize

  /* syntax sugar */

  class ~~~>[@specialized S] private[coroutines] (
    val blueprint: Coroutine.Blueprint[S]
  ) extends Coroutine.DefMarker[S] {
    def apply(): S =
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call(): Coroutine.Inst[S] =
      blueprint.asInstanceOf[Coroutine._0[S]].$call()
    def $push(co: Coroutine.Inst[S]): Unit =
      blueprint.asInstanceOf[Coroutine._0[S]].$push(co)
  }

  class ~~>[T, @specialized S] private[coroutines] (
    val blueprint: Coroutine.Blueprint[S]
  ) extends Coroutine.DefMarker[S] {
    def apply(t: T): S =
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call(t: T): Coroutine.Inst[S] =
      blueprint.asInstanceOf[Coroutine._1[T, S]].$call(t)
    def $push(co: Coroutine.Inst[S], t: T): Unit =
      blueprint.asInstanceOf[Coroutine._1[T, S]].$push(co, t)
  }

  class ~>[PS, @specialized S] private[coroutines] (
    val blueprint: Coroutine.Blueprint[S]
  ) extends Coroutine.DefMarker[S] {
    def apply[T1, T2](t1: T1, t2: T2)(
      implicit e: PS =:= Tuple2[T1, T2]
    ): S = {
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    }
    def $call[T1, T2](t1: T1, t2: T2)(
      implicit e: PS =:= Tuple2[T1, T2]
    ): Coroutine.Inst[S] = {
      blueprint.asInstanceOf[Coroutine._2[T1, T2, S]].$call(t1, t2)
    }
    def $push[T1, T2](co: Coroutine.Inst[S], t1: T1, t2: T2)(
      implicit e: PS =:= Tuple2[T1, T2]
    ): Unit = {
      blueprint.asInstanceOf[Coroutine._2[T1, T2, S]].$push(co, t1, t2)
    }
    def apply[T1, T2, T3](t1: T1, t2: T2, t3: T3)(
      implicit e: PS =:= Tuple3[T1, T2, T3]
    ): S = {
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    }
    def $call[T1, T2, T3](t1: T1, t2: T2, t3: T3)(
      implicit e: PS =:= Tuple3[T1, T2, T3]
    ): Coroutine.Inst[S] = {
      blueprint.asInstanceOf[Coroutine._3[T1, T2, T3, S]].$call(t1, t2, t3)
    }
    def $push[T1, T2, T3](co: Coroutine.Inst[S], t1: T1, t2: T2, t3: T3)(
      implicit e: PS =:= Tuple3[T1, T2, T3]
    ): Unit = {
      blueprint.asInstanceOf[Coroutine._3[T1, T2, T3, S]].$push(co, t1, t2, t3)
    }
  }

  implicit def coroutine0[@specialized S](b: Coroutine._0[S]) =
    new ~~~>[S](b)

  implicit def coroutine1[T, @specialized S](b: Coroutine._1[T, S]) =
    new ~~>[T, S](b)

  implicit def coroutine2[T1, T2, @specialized S](b: Coroutine._2[T1, T2, S]) =
    new ~>[Tuple2[T1, T2], S](b)

  implicit def coroutine3[T1, T2, T3, @specialized S](b: Coroutine._3[T1, T2, T3, S]) =
    new ~>[Tuple3[T1, T2, T3], S](b)

  class coroutine2ops[T1, T2, @specialized S](val c: (T1, T2) ~> S)
  extends Coroutine.DefMarker[S] {
    
  }

  implicit def coroutine2ops[T1, T2, @specialized S](c: (T1, T2) ~> S) =
    new coroutine2ops(c)

  class coroutine3ops[T1, T2, T3, @specialized S](val c: (T1, T2, T3) ~> S)
  extends Coroutine.DefMarker[S] {
    
  }

  implicit def coroutine3ops[T1, T2, T3, @specialized S](c: (T1, T2, T3) ~> S) =
    new coroutine3ops(c)

}
