package scala.coroutines






abstract class _1$spec$Z[@specialized T] extends Coroutine._1[Boolean, T] {
  def apply(a0: Boolean): T
  def $call(a0: Boolean): Coroutine[T]
  def $push(c: Coroutine[T], a0: Boolean): Unit
}


abstract class _1$spec$B[@specialized T] extends Coroutine._1[Byte, T] {
  def apply(a0: Byte): T
  def $call(a0: Byte): Coroutine[T]
  def $push(c: Coroutine[T], a0: Byte): Unit
}


abstract class _1$spec$S[@specialized T] extends Coroutine._1[Short, T] {
  def apply(a0: Short): T
  def $call(a0: Short): Coroutine[T]
  def $push(c: Coroutine[T], a0: Short): Unit
}


abstract class _1$spec$C[@specialized T] extends Coroutine._1[Char, T] {
  def apply(a0: Char): T
  def $call(a0: Char): Coroutine[T]
  def $push(c: Coroutine[T], a0: Char): Unit
}


abstract class _1$spec$I[@specialized T] extends Coroutine._1[Int, T] {
  def apply(a0: Int): T
  def $call(a0: Int): Coroutine[T]
  def $push(c: Coroutine[T], a0: Int): Unit
}


abstract class _1$spec$F[@specialized T] extends Coroutine._1[Float, T] {
  def apply(a0: Float): T
  def $call(a0: Float): Coroutine[T]
  def $push(c: Coroutine[T], a0: Float): Unit
}


abstract class _1$spec$J[@specialized T] extends Coroutine._1[Long, T] {
  def apply(a0: Long): T
  def $call(a0: Long): Coroutine[T]
  def $push(c: Coroutine[T], a0: Long): Unit
}


abstract class _1$spec$D[@specialized T] extends Coroutine._1[Double, T] {
  def apply(a0: Double): T
  def $call(a0: Double): Coroutine[T]
  def $push(c: Coroutine[T], a0: Double): Unit
}

abstract class _1$spec$L[S, @specialized T] extends Coroutine._1[S, T] {
  def apply(a0: S): T
  def $call(a0: S): Coroutine[T]
  def $push(c: Coroutine[T], a0: S): Unit
}
