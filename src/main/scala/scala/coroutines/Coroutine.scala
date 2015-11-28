package scala.coroutines



import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.util.Try



class Coroutine[@specialized +T] {
  import Coroutine._
  private[coroutines] var costackptr = 0
  private[coroutines] var costack: Array[Blueprint[T]] @uncheckedVariance =
    new Array[Blueprint[T]](INITIAL_COSTACK_SIZE)
  private[coroutines] var pcstackptr = 0
  private[coroutines] var pcstack = new Array[Short](INITIAL_COSTACK_SIZE)
  private[coroutines] var refstackptr = 0
  private[coroutines] var refstack: Array[AnyRef] = _
  private[coroutines] var valstackptr = 0
  private[coroutines] var valstack: Array[Long] = _
  private[coroutines] var target: Coroutine[T] @uncheckedVariance = null
  private[coroutines] var result: T @uncheckedVariance = null.asInstanceOf[T]

  def apply(): T = Coroutine.enter[T](this)

  def get(): Option[T] = ???

  def tryGet(): Try[T] = ???
}


object Coroutine {
  private[coroutines] val INITIAL_COSTACK_SIZE = 4

  @tailrec
  private[coroutines] final def enter[T](c: Coroutine[T]): T = {
    val cd = Stack.top(c.costack)
    cd.enter(c)
    if (c.target ne null) {
      val nc = c.target
      c.target = null
      enter(nc)
    } else {
      val res = c.result
      c.result = null.asInstanceOf[T]
      res
    }
  }

  abstract class Blueprint[T] {
    def enter(c: Coroutine[T]): Unit
    def returnvalue(c: Coroutine[T], v: T)(implicit cc: CanCallInternal): Unit
  }

  def synthesize(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).synthesize(f)
  }

  def call[T: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).call(f)
  }

  abstract class Arity0[@specialized T] extends Coroutine.Blueprint[T] {
    def call()(implicit cc: CanCallInternal): Coroutine[T]
    def apply(): T
    def push(c: Coroutine[T])(implicit cc: CanCallInternal): Unit
  }

  abstract class Arity1[A0, @specialized T] extends Coroutine.Blueprint[T] {
    def call(a0: A0)(implicit cc: CanCallInternal): Coroutine[T]
    def apply(a0: A0): T
    def push(c: Coroutine[T], a0: A0)(implicit cc: CanCallInternal): Unit
  }

  abstract class Arity2[A0, A1, @specialized T] extends Coroutine.Blueprint[T] {
    def call(a0: A0, a1: A1)(implicit cc: CanCallInternal): Coroutine[T]
    def apply(a0: A0, a1: A1): T
    def push(c: Coroutine[T], a0: A0, a1: A1)(implicit cc: CanCallInternal): Unit
  }

  abstract class Arity3[A0, A1, A2, @specialized T] extends Coroutine.Blueprint[T] {
    def call(a0: A0, a1: A1, a2: A2)(implicit cc: CanCallInternal): Coroutine[T]
    def apply(a0: A0, a1: A1, a2: A2): T
    def push(c: Coroutine[T], a0: A0, a1: A1, a2: A2)(
      implicit cc: CanCallInternal
    ): Unit
  }
}
