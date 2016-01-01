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



trait Coroutine[@specialized T, R] extends Coroutine.DefMarker[T, R] {
  def $enter(c: Coroutine.Inst[T, R]): Unit
  def $assignyield(c: Coroutine.Inst[T, R], v: T): Unit = c.$yield = v
  def $assignresult(c: Coroutine.Inst[T, R], v: R): Unit = c.$result = v
  def $returnvalue(c: Coroutine.Inst[T, R], v: R): Unit
  def $ep0(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep1(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep2(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep3(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep4(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep5(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep6(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep7(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep8(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep9(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep10(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep11(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep12(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep13(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep14(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep15(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep16(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep17(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep18(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep19(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep20(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep21(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep22(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep23(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep24(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep25(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep26(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep27(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep28(c: Coroutine.Inst[T, R]): Unit = {}
  def $ep29(c: Coroutine.Inst[T, R]): Unit = {}
}


object Coroutine {
  private[coroutines] val INITIAL_COSTACK_SIZE = 4

  @tailrec
  private[coroutines] final def resume[T, R](
    callsite: Inst[T, R], actual: Inst[T, R]
  ): Boolean = {
    val cd = Stack.top(actual.$costack)
    cd.$enter(actual)
    val resumeStatus = actual.isLive
    if (actual.$target ne null) {
      val newactual = actual.$target
      actual.$target = null
      resume(callsite, newactual.asInstanceOf[callsite.type])
    } else if (actual.$exception ne null) {
      val e = actual.$exception
      actual.$exception = null
      throw e
    } else {
      if (resumeStatus) callsite.$yield = actual.$yield
      resumeStatus
    }
  }

  class Inst[@specialized T, R] {
    var $costackptr = 0
    var $costack: Array[Coroutine[T, R]] =
      new Array[Coroutine[T, R]](INITIAL_COSTACK_SIZE)
    var $pcstackptr = 0
    var $pcstack = new Array[Short](INITIAL_COSTACK_SIZE)
    var $refstackptr = 0
    var $refstack: Array[AnyRef] = _
    var $valstackptr = 0
    var $valstack: Array[Int] = _
    var $target: Inst[T, _] = null
    var $exception: Throwable = null
    var $yield: T = null.asInstanceOf[T]
    var $result: R = null.asInstanceOf[R]

    def resume: Boolean = {
      if (isLive) Coroutine.resume[T, R](this, this)
      else throw new CoroutineStoppedException
    }

    def value: T = {
      if (nonResumed)
        sys.error("Coroutine has no value, because it was not resumed yet.")
      if (!isLive)
        sys.error("Coroutine has no value, because it is completed.")
      $yield
    }

    def getValue = if (!nonResumed && isLive) Some(value) else None

    def tryValue = try { Success(value) } catch { case t: Throwable => Failure(t) }

    def result: R = {
      if (!isCompleted)
        sys.error("Coroutine has no result, because it is not completed.")
      $result
    }

    def getResult = if (isCompleted) Some(result) else None

    def tryResult = try { Success(result) } catch { case t: Throwable => Failure(t) }

    def nonResumed: Boolean =
      $pcstackptr > 0 && scala.coroutines.common.Stack.top($pcstack) == 0

    def isLive: Boolean = $costackptr > 0

    def isCompleted: Boolean = !isLive
  }

  trait DefMarker[@specialized T, R]

  def synthesize(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).synthesize(f)
  }

  def call[T: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).call(f)
  }

  abstract class _0[@specialized T, R] extends Coroutine[T, R] {
    def apply(): R
    def $call(): Inst[T, R]
    def $push(c: Inst[T, R]): Unit
  }

  abstract class _1[A0, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0): R
    def $call(a0: A0): Inst[T, R]
    def $push(c: Inst[T, R], a0: A0): Unit
  }

  abstract class _2[A0, A1, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0, a1: A1): R
    def $call(a0: A0, a1: A1): Inst[T, R]
    def $push(c: Inst[T, R], a0: A0, a1: A1): Unit
  }

  abstract class _3[A0, A1, A2, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0, a1: A1, a2: A2): R
    def $call(a0: A0, a1: A1, a2: A2): Inst[T, R]
    def $push(c: Inst[T, R], a0: A0, a1: A1, a2: A2): Unit
  }
}
