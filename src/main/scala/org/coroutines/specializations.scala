package org.coroutines






/* Coroutine._1 specializations */

trait _1$spec$S[@specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[Short, S, R] {
  def apply(a0: Short): R
  def $call(a0: Short): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Short): Unit
}


trait _1$spec$C[@specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[Char, S, R] {
  def apply(a0: Char): R
  def $call(a0: Char): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Char): Unit
}


trait _1$spec$I[@specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[Int, S, R] {
  def apply(a0: Int): R
  def $call(a0: Int): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Int): Unit
}


trait _1$spec$F[@specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[Float, S, R] {
  def apply(a0: Float): R
  def $call(a0: Float): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Float): Unit
}


trait _1$spec$J[@specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[Long, S, R] {
  def apply(a0: Long): R
  def $call(a0: Long): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Long): Unit
}


trait _1$spec$D[@specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[Double, S, R] {
  def apply(a0: Double): R
  def $call(a0: Double): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Double): Unit
}

trait _1$spec$L[T0, @specialized(Short, Char, Int, Float, Long, Double) S, R]
extends Coroutine._1[T0, S, R] {
  def apply(a0: T0): R
  def $call(a0: T0): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: T0): Unit
}

/* Coroutine._2 specializations. */

trait _2$spec$II[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Int, Int, S, R] {
  def apply(a0: Int, a1: Int): R
  def $call(a0: Int, a1: Int): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Int, a1: Int): Unit
}

trait _2$spec$JI[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Long, Int, S, R] {
  def apply(a0: Long, a1: Int): R
  def $call(a0: Long, a1: Int): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Long, a1: Int): Unit
}

trait _2$spec$DI[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Double, Int, S, R] {
  def apply(a0: Double, a1: Int): R
  def $call(a0: Double, a1: Int): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Double, a1: Int): Unit
}

trait _2$spec$LI[T0, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[T0, Int, S, R] {
  def apply(a0: T0, a1: Int): R
  def $call(a0: T0, a1: Int): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: T0, a1: Int): Unit
}

trait _2$spec$IJ[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Int, Long, S, R] {
  def apply(a0: Int, a1: Long): R
  def $call(a0: Int, a1: Long): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Int, a1: Long): Unit
}

trait _2$spec$JJ[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Long, Long, S, R] {
  def apply(a0: Long, a1: Long): R
  def $call(a0: Long, a1: Long): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Long, a1: Long): Unit
}

trait _2$spec$DJ[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Double, Long, S, R] {
  def apply(a0: Double, a1: Long): R
  def $call(a0: Double, a1: Long): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Double, a1: Long): Unit
}

trait _2$spec$LJ[T0, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[T0, Long, S, R] {
  def apply(a0: T0, a1: Long): R
  def $call(a0: T0, a1: Long): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: T0, a1: Long): Unit
}

trait _2$spec$ID[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Int, Double, S, R] {
  def apply(a0: Int, a1: Double): R
  def $call(a0: Int, a1: Double): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Int, a1: Double): Unit
}

trait _2$spec$JD[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Long, Double, S, R] {
  def apply(a0: Long, a1: Double): R
  def $call(a0: Long, a1: Double): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Long, a1: Double): Unit
}

trait _2$spec$DD[@specialized(Int, Long, Double) S, R]
extends Coroutine._2[Double, Double, S, R] {
  def apply(a0: Double, a1: Double): R
  def $call(a0: Double, a1: Double): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Double, a1: Double): Unit
}

trait _2$spec$LD[T0, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[T0, Double, S, R] {
  def apply(a0: T0, a1: Double): R
  def $call(a0: T0, a1: Double): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: T0, a1: Double): Unit
}

trait _2$spec$IL[T1, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[Int, T1, S, R] {
  def apply(a0: Int, a1: T1): R
  def $call(a0: Int, a1: T1): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Int, a1: T1): Unit
}

trait _2$spec$JL[T1, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[Long, T1, S, R] {
  def apply(a0: Long, a1: T1): R
  def $call(a0: Long, a1: T1): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Long, a1: T1): Unit
}

trait _2$spec$DL[T1, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[Double, T1, S, R] {
  def apply(a0: Double, a1: T1): R
  def $call(a0: Double, a1: T1): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: Double, a1: T1): Unit
}

trait _2$spec$LL[T0, T1, @specialized(Int, Long, Double) S, R]
extends Coroutine._2[T0, T1, S, R] {
  def apply(a0: T0, a1: T1): R
  def $call(a0: T0, a1: T1): Coroutine.Instance[S, R]
  def $push(c: Coroutine.Instance[S, R], a0: T0, a1: T1): Unit
}
