package scala.coroutines



import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.util.Failure
import scala.util.Success
import scala.util.Try



trait Coroutine[@specialized T] extends Coroutine.DefMarker[T] {
  def $enter(c: Coroutine.Inst[T]): Unit
  def $assignresult(c: Coroutine.Inst[T], v: T): Unit = c.$yield = v
  def $returnvalue(c: Coroutine.Inst[T], v: T): Unit
  def $ep0(c: Coroutine.Inst[T]): Unit = {}
  def $ep1(c: Coroutine.Inst[T]): Unit = {}
  def $ep2(c: Coroutine.Inst[T]): Unit = {}
  def $ep3(c: Coroutine.Inst[T]): Unit = {}
  def $ep4(c: Coroutine.Inst[T]): Unit = {}
  def $ep5(c: Coroutine.Inst[T]): Unit = {}
  def $ep6(c: Coroutine.Inst[T]): Unit = {}
  def $ep7(c: Coroutine.Inst[T]): Unit = {}
  def $ep8(c: Coroutine.Inst[T]): Unit = {}
  def $ep9(c: Coroutine.Inst[T]): Unit = {}
  def $ep10(c: Coroutine.Inst[T]): Unit = {}
  def $ep11(c: Coroutine.Inst[T]): Unit = {}
  def $ep12(c: Coroutine.Inst[T]): Unit = {}
  def $ep13(c: Coroutine.Inst[T]): Unit = {}
  def $ep14(c: Coroutine.Inst[T]): Unit = {}
  def $ep15(c: Coroutine.Inst[T]): Unit = {}
  def $ep16(c: Coroutine.Inst[T]): Unit = {}
  def $ep17(c: Coroutine.Inst[T]): Unit = {}
  def $ep18(c: Coroutine.Inst[T]): Unit = {}
  def $ep19(c: Coroutine.Inst[T]): Unit = {}
  def $ep20(c: Coroutine.Inst[T]): Unit = {}
  def $ep21(c: Coroutine.Inst[T]): Unit = {}
  def $ep22(c: Coroutine.Inst[T]): Unit = {}
  def $ep23(c: Coroutine.Inst[T]): Unit = {}
  def $ep24(c: Coroutine.Inst[T]): Unit = {}
  def $ep25(c: Coroutine.Inst[T]): Unit = {}
  def $ep26(c: Coroutine.Inst[T]): Unit = {}
  def $ep27(c: Coroutine.Inst[T]): Unit = {}
  def $ep28(c: Coroutine.Inst[T]): Unit = {}
  def $ep29(c: Coroutine.Inst[T]): Unit = {}
}


object Coroutine {
  private[coroutines] val INITIAL_COSTACK_SIZE = 4

  @tailrec
  private[coroutines] final def enter[T](c: Inst[T]): T = {
    val cd = Stack.top(c.$costack)
    cd.$enter(c)
    if (c.$target ne null) {
      val nc = c.$target
      c.$target = null
      enter(nc)
    } else if (c.$exception ne null) {
      val e = c.$exception
      c.$exception = null
      throw e
    } else {
      val res = c.$yield
      c.$yield = null.asInstanceOf[T]
      res
    }
  }

  class Inst[@specialized T] {
    var $costackptr = 0
    var $costack: Array[Coroutine[T]] = new Array[Coroutine[T]](INITIAL_COSTACK_SIZE)
    var $pcstackptr = 0
    var $pcstack = new Array[Short](INITIAL_COSTACK_SIZE)
    var $refstackptr = 0
    var $refstack: Array[AnyRef] = _
    var $valstackptr = 0
    var $valstack: Array[Int] = _
    var $target: Inst[T] = null
    var $exception: Throwable = null
    var $yield: T = null.asInstanceOf[T]

    def apply(): T = {
      if (isAlive) Coroutine.enter[T](this)
      else throw new CoroutineStoppedException
    }

    def isAlive: Boolean = $costackptr > 0

    def isStopped: Boolean = !isAlive

    def get(): Option[T] = if (isAlive) Some(apply()) else None

    def tryGet(): Try[T] = {
      try Success(apply())
      catch {
        case t: Throwable => Failure(t)
      }
    }
  }

  trait DefMarker[@specialized T]

  def synthesize(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).synthesize(f)
  }

  def call[T: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).call(f)
  }

  abstract class _0[@specialized T] extends Coroutine[T] {
    def apply(): T
    def $call(): Inst[T]
    def $push(c: Inst[T]): Unit
  }

  abstract class _1[A0, @specialized T] extends Coroutine[T] {
    def apply(a0: A0): T
    def $call(a0: A0): Inst[T]
    def $push(c: Inst[T], a0: A0): Unit
  }

  abstract class _2[A0, A1, @specialized T] extends Coroutine[T] {
    def apply(a0: A0, a1: A1): T
    def $call(a0: A0, a1: A1): Inst[T]
    def $push(c: Inst[T], a0: A0, a1: A1): Unit
  }

  abstract class _3[A0, A1, A2, @specialized T] extends Coroutine[T] {
    def apply(a0: A0, a1: A1, a2: A2): T
    def $call(a0: A0, a1: A1, a2: A2): Inst[T]
    def $push(c: Inst[T], a0: A0, a1: A1, a2: A2): Unit
  }
}
