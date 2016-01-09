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



trait Coroutine[@specialized Y, R] extends Coroutine.DefMarker[(Y, R)] {
  def $enter(c: Coroutine.Frame[Y, R]): Unit
  def $assignyield(c: Coroutine.Frame[Y, R], v: Y): Unit = {
    c.$hasYield = true
    c.$yield = v
  }
  def $assignresult(c: Coroutine.Frame[Y, R], v: R): Unit = c.$result = v
  def $returnvalue$Z(c: scala.coroutines.Coroutine.Frame[Y, R], v: Boolean): Unit
  def $returnvalue$B(c: scala.coroutines.Coroutine.Frame[Y, R], v: Byte): Unit
  def $returnvalue$S(c: scala.coroutines.Coroutine.Frame[Y, R], v: Short): Unit
  def $returnvalue$C(c: scala.coroutines.Coroutine.Frame[Y, R], v: Char): Unit
  def $returnvalue$I(c: scala.coroutines.Coroutine.Frame[Y, R], v: Int): Unit
  def $returnvalue$F(c: scala.coroutines.Coroutine.Frame[Y, R], v: Float): Unit
  def $returnvalue$J(c: scala.coroutines.Coroutine.Frame[Y, R], v: Long): Unit
  def $returnvalue$D(c: scala.coroutines.Coroutine.Frame[Y, R], v: Double): Unit
  def $returnvalue$L(c: scala.coroutines.Coroutine.Frame[Y, R], v: Any): Unit
  def $ep0(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep1(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep2(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep3(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep4(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep5(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep6(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep7(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep8(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep9(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep10(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep11(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep12(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep13(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep14(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep15(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep16(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep17(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep18(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep19(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep20(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep21(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep22(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep23(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep24(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep25(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep26(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep27(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep28(c: Coroutine.Frame[Y, R]): Unit = {}
  def $ep29(c: Coroutine.Frame[Y, R]): Unit = {}
}


object Coroutine {
  private[coroutines] val INITIAL_COSTACK_SIZE = 4

  type SomeY

  type SomeR

  @tailrec
  private[coroutines] final def resume[Y, R](
    callsite: Frame[Y, R], actual: Frame[_, _]
  ): Boolean = {
    val cd = Stack.top(actual.$costack).asInstanceOf[Coroutine[SomeY, SomeR]]
    cd.$enter(actual.asInstanceOf[Frame[SomeY, SomeR]])
    val resumeStatus = actual.isLive
    if (actual.$target ne null) {
      val newactual = actual.$target
      actual.$target = null
      resume(callsite, newactual)
    } else if (actual.$exception ne null) {
      callsite.isLive
    } else {
      callsite.isLive
    }
  }

  class Frame[@specialized T, R] {
    var $costackptr = 0
    var $costack: Array[Coroutine[T, R]] =
      new Array[Coroutine[T, R]](INITIAL_COSTACK_SIZE)
    var $pcstackptr = 0
    var $pcstack = new Array[Short](INITIAL_COSTACK_SIZE)
    var $refstackptr = 0
    var $refstack: Array[AnyRef] = _
    var $valstackptr = 0
    var $valstack: Array[Int] = _
    var $target: Frame[T, _] = null
    var $exception: Throwable = null
    var $hasYield: Boolean = false
    var $yield: T = null.asInstanceOf[T]
    var $result: R = null.asInstanceOf[R]

    final def resume: Boolean = {
      if (isLive) {
        $hasYield = false
        $yield = null.asInstanceOf[T]
        Coroutine.resume[T, R](this, this)
      } else throw new CoroutineStoppedException
    }

    final def value: T = {
      if (!hasValue)
        sys.error("Coroutine has no value, because it did not yield.")
      if (!isLive)
        sys.error("Coroutine has no value, because it is completed.")
      $yield
    }

    final def hasValue = $hasYield

    final def getValue = if (hasValue) Some(value) else None

    final def tryValue = try { Success(value) } catch { case t: Throwable => Failure(t) }

    final def result: R = {
      if (!isCompleted)
        sys.error("Coroutine has no result, because it is not completed.")
      if ($exception != null) throw $exception
      $result
    }

    final def hasResult = isCompleted && $exception == null

    final def getResult = if (hasResult) Some(result) else None

    final def tryResult = {
      if ($exception != null) Failure($exception)
      else Try(result)
    }

    final def isLive: Boolean = $costackptr > 0

    final def isCompleted: Boolean = !isLive

    override def toString = s"Coroutine.Frame<depth: ${$costackptr}, live: $isLive>"
  }

  trait DefMarker[YR]

  def synthesize(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).synthesize(f)
  }

  def call[T: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).call(f)
  }

  abstract class _0[@specialized T, R] extends Coroutine[T, R] {
    def apply(): R
    def $call(): Frame[T, R]
    def $push(c: Frame[T, R]): Unit
    override def toString = s"Coroutine._0@${System.identityHashCode(this)}"
  }

  abstract class _1[A0, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0): R
    def $call(a0: A0): Frame[T, R]
    def $push(c: Frame[T, R], a0: A0): Unit
    override def toString = s"Coroutine._1@${System.identityHashCode(this)}"
  }

  abstract class _2[A0, A1, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0, a1: A1): R
    def $call(a0: A0, a1: A1): Frame[T, R]
    def $push(c: Frame[T, R], a0: A0, a1: A1): Unit
    override def toString = s"Coroutine._2@${System.identityHashCode(this)}"
  }

  abstract class _3[A0, A1, A2, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0, a1: A1, a2: A2): R
    def $call(a0: A0, a1: A1, a2: A2): Frame[T, R]
    def $push(c: Frame[T, R], a0: A0, a1: A1, a2: A2): Unit
    override def toString = s"Coroutine._3@${System.identityHashCode(this)}"
  }
}
