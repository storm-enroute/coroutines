package org



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

  def yieldto[T](f: Coroutine.Frame[T, _]): Unit = {
    sys.error("Yield allowed only inside coroutines.")
  }

  def call[R](f: R): Any = macro Coroutine.call[R]

  def coroutine[T](f: Any): Any = macro Coroutine.synthesize

  /* syntax sugar */

  type <~>[Y, R] = Coroutine.Frame[Y, R]

  class ~~~>[@specialized S, R] private[coroutines] (
    val blueprint: Coroutine[S, R]
  ) extends Coroutine.DefMarker[(S, R)] {
    def apply(): R =
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call(): Coroutine.Frame[S, R] =
      blueprint.asInstanceOf[Coroutine._0[S, R]].$call()
    def $push(co: Coroutine.Frame[S, R]): Unit =
      blueprint.asInstanceOf[Coroutine._0[S, R]].$push(co)
  }

  class ~~>[T, YR] private[coroutines] (
    val blueprint: Coroutine.DefMarker[YR]
  ) extends Coroutine.DefMarker[YR] {
    def apply[@specialized S, R](t: T)(implicit e: (S, R) =:= YR): R =
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    def $call[@specialized S, R](t: T)(
      implicit e: (S, R) =:= YR
    ): Coroutine.Frame[S, R] = {
      blueprint.asInstanceOf[Coroutine._1[T, S, R]].$call(t)
    }
    def $push[@specialized S, R](co: Coroutine.Frame[S, R], t: T)(
      implicit e: (S, R) =:= YR
    ): Unit = {
      blueprint.asInstanceOf[Coroutine._1[T, S, R]].$push(co, t)
    }
  }

  class ~>[PS, YR] private[coroutines] (
    val blueprint: Coroutine.DefMarker[YR]
  ) extends Coroutine.DefMarker[YR] {
    def apply[T1, T2, @specialized S, R](t1: T1, t2: T2)(
      implicit ps: PS =:= Tuple2[T1, T2], yr: (S, R) =:= YR
    ): R = {
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    }
    def $call[T1, T2, @specialized S, R](t1: T1, t2: T2)(
      implicit ps: PS =:= Tuple2[T1, T2], yr: (S, R) =:= YR
    ): Coroutine.Frame[S, R] = {
      blueprint.asInstanceOf[Coroutine._2[T1, T2, S, R]].$call(t1, t2)
    }
    def $push[T1, T2, @specialized S, R](co: Coroutine.Frame[S, R], t1: T1, t2: T2)(
      implicit ps: PS =:= Tuple2[T1, T2], yr: (S, R) =:= YR
    ): Unit = {
      blueprint.asInstanceOf[Coroutine._2[T1, T2, S, R]].$push(co, t1, t2)
    }
    def apply[T1, T2, T3, @specialized S, R](t1: T1, t2: T2, t3: T3)(
      implicit ps: PS =:= Tuple3[T1, T2, T3], yr: (S, R) =:= YR
    ): R = {
      sys.error(COROUTINE_DIRECT_APPLY_ERROR_MESSAGE)
    }
    def $call[T1, T2, T3, @specialized S, R](t1: T1, t2: T2, t3: T3)(
      implicit ps: PS =:= Tuple3[T1, T2, T3], yr: (S, R) =:= YR
    ): Coroutine.Frame[S, R] = {
      blueprint.asInstanceOf[Coroutine._3[T1, T2, T3, S, R]].$call(t1, t2, t3)
    }
    def $push[T1, T2, T3, @specialized S, R](
      co: Coroutine.Frame[S, R], t1: T1, t2: T2, t3: T3
    )(
      implicit ps: PS =:= Tuple3[T1, T2, T3], yr: (S, R) =:= YR
    ): Unit = {
      blueprint.asInstanceOf[Coroutine._3[T1, T2, T3, S, R]].$push(co, t1, t2, t3)
    }
  }

  implicit def coroutine0nothing[R](b: Coroutine._0[Nothing, R]) =
    new ~~~>[Nothing, R](b)

  implicit def coroutine0[@specialized S, R](b: Coroutine._0[S, R]) =
    new ~~~>[S, R](b)

  implicit def coroutine1nothing[T, R](b: Coroutine._1[T, Nothing, R]) =
    new ~~>[T, (Nothing, R)](b)

  implicit def coroutine1[T, @specialized S, R](b: Coroutine._1[T, S, R]) =
    new ~~>[T, (S, R)](b)

  implicit def coroutine2nothing[T1, T2, R](
    b: Coroutine._2[T1, T2, Nothing, R]
  ) = {
    new ~>[Tuple2[T1, T2], (Nothing, R)](b)
  }

  implicit def coroutine2[T1, T2, @specialized S, R](b: Coroutine._2[T1, T2, S, R]) =
    new ~>[Tuple2[T1, T2], (S, R)](b)

  implicit def coroutine3nothing[T1, T2, T3, R](
    b: Coroutine._3[T1, T2, T3, Nothing, R]
  ) = {
    new ~>[Tuple3[T1, T2, T3], (Nothing, R)](b)
  }

  implicit def coroutine3[T1, T2, T3, @specialized S, R](
    b: Coroutine._3[T1, T2, T3, S, R]
  ) = {
    new ~>[Tuple3[T1, T2, T3], (S, R)](b)
  }
}
