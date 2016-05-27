package org.coroutines



import org.coroutines.common._
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.util.Failure
import scala.util.Success
import scala.util.Try



trait Coroutine[@specialized Y, R] extends Coroutine.DefMarker[(Y, R)] {
  def $enter(c: Coroutine.Instance[Y, R]): Unit
  def $assignyield(c: Coroutine.Instance[Y, R], v: Y): Unit = {
    c.$hasYield = true
    c.$yield = v
  }
  def $assignresult(c: Coroutine.Instance[Y, R], v: R): Unit = c.$result = v
  def $returnvalue$Z(c: Coroutine.Instance[Y, R], v: Boolean): Unit
  def $returnvalue$B(c: Coroutine.Instance[Y, R], v: Byte): Unit
  def $returnvalue$S(c: Coroutine.Instance[Y, R], v: Short): Unit
  def $returnvalue$C(c: Coroutine.Instance[Y, R], v: Char): Unit
  def $returnvalue$I(c: Coroutine.Instance[Y, R], v: Int): Unit
  def $returnvalue$F(c: Coroutine.Instance[Y, R], v: Float): Unit
  def $returnvalue$J(c: Coroutine.Instance[Y, R], v: Long): Unit
  def $returnvalue$D(c: Coroutine.Instance[Y, R], v: Double): Unit
  def $returnvalue$L(c: Coroutine.Instance[Y, R], v: Any): Unit
  def $ep0(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep1(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep2(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep3(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep4(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep5(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep6(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep7(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep8(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep9(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep10(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep11(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep12(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep13(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep14(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep15(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep16(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep17(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep18(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep19(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep20(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep21(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep22(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep23(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep24(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep25(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep26(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep27(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep28(c: Coroutine.Instance[Y, R]): Unit = {}
  def $ep29(c: Coroutine.Instance[Y, R]): Unit = {}
}

object Coroutine {
  private[coroutines] val INITIAL_COSTACK_SIZE = 4

  type SomeY

  type SomeR

  @tailrec
  private[coroutines] final def resume[Y, R](
    callsite: Instance[Y, R], actual: Instance[_, _]
  ): Boolean = {
    val cd = Stack.top(actual.$costack).asInstanceOf[Coroutine[SomeY, SomeR]]
    cd.$enter(actual.asInstanceOf[Instance[SomeY, SomeR]])
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

  class Instance[@specialized Y, R] {
    var $costackptr = 0
    var $costack: Array[Coroutine[Y, R]] =
      new Array[Coroutine[Y, R]](INITIAL_COSTACK_SIZE)
    var $pcstackptr = 0
    var $pcstack = new Array[Short](INITIAL_COSTACK_SIZE)
    var $refstackptr = 0
    var $refstack: Array[AnyRef] = _
    var $valstackptr = 0
    var $valstack: Array[Int] = _
    var $target: Instance[Y, _] = null
    var $exception: Throwable = null
    var $hasYield: Boolean = false
    var $yield: Y = null.asInstanceOf[Y]
    var $result: R = null.asInstanceOf[R]

    /**
      * Clones the coroutine that this instance is a part of
      * @return A new instance with the same type arguments, stacks, and internal
      *         values. 
      */
    final def snapshot: Instance[Y, R] = {
      val frame = new Instance[Y, R]
      Stack.copy(this.$costack, frame.$costack)
      Stack.copy(this.$pcstack, frame.$pcstack)
      Stack.copy(this.$refstack, frame.$refstack)
      Stack.copy(this.$valstack, frame.$valstack)
      frame.$exception = this.$exception
      frame.$hasYield = this.$hasYield
      frame.$yield = this.$yield
      frame.$result = this.$result
      frame
    }

    /**
      * Advances the coroutine to the next yielding point
      * @return If resume can be called again
      * @throws CoroutineStoppedException if the coroutine is not live (cannot
      *                                   be resumed)
      */
    final def resume: Boolean = {
      if (isLive) {
        $hasYield = false
        $yield = null.asInstanceOf[Y]
        Coroutine.resume[Y, R](this, this)
      } else throw new CoroutineStoppedException
    }

    /**
      * Usage:
      * {{{
      * while (c.pull) c.value
      * }}}
      * @return Whether or not the coroutine has a next value that is defined
      * @throws CoroutineStoppedException if the coroutine is not live 
      */
    @tailrec
    final def pull: Boolean = {
      if (isLive) {
        if (!resume) false
        else if (hasValue) true
        else pull
      } else throw new CoroutineStoppedException
    }

    /**
      * @return The yield value of the coroutine, if there is one
      * @throws RuntimeException If the coroutine doesn't have a value or if it
      *                         is not live
      */
    final def value: Y = {
      if (!hasValue)
        sys.error("Coroutine has no value, because it did not yield.")
      if (!isLive)
        sys.error("Coroutine has no value, because it is completed.")
      $yield
    }

    final def hasValue: Boolean = $hasYield

    final def getValue: Option[Y] = if (hasValue) Some(value) else None

    final def tryValue: Try[Y] =
      try { Success(value) } catch { case t: Throwable => Failure(t) }

    /**
      * @return The return value of the coroutine, if it is completed.
      * @throws RuntimeException If the coroutine has not been completed
      * @throws Exception        If <code>\$exception</code> is not null
      */
    final def result: R = {
      if (!isCompleted)
        sys.error("Coroutine has no result, because it is not completed.")
      if ($exception != null) throw $exception
      $result
    }

    final def hasResult: Boolean = isCompleted && $exception == null

    final def getResult: Option[R] = if (hasResult) Some(result) else None

    final def tryResult: Try[R] = {
      if ($exception != null) Failure($exception)
      else Try(result)
    }

    final def hasException: Boolean = isCompleted && $exception != null

    /**
      * @return If <code>resume</code> can be called without an exception being
      *         thrown
      */
    final def isLive: Boolean = $costackptr > 0

    /**
      * @return The inverse of <code>isLive</code>
      */
    final def isCompleted: Boolean = !isLive

    /**
      * @return A string containing the values of <code>\$costackptr</code> and
      *         and <code>\$isLive</code>
      */
    override def toString = s"Coroutine.Instance<depth: ${$costackptr}, live: $isLive>"


    final def debugString: String = {
      def toStackLength[T](stack: Array[T]) =
        if (stack != null) "${stack.length}" else "<uninitialized>"
      def toStackString[T](stack: Array[T]) =
        if (stack != null) stack.mkString("[", ", ", "]") else "<uninitialized>"
      s"Coroutine.Instance <\n" +
      s"  costackptr:  ${$costackptr}\n" +
      s"  costack sz:  ${toStackLength($costack)}\n" +
      s"  pcstackptr:  ${$pcstackptr}\n" +
      s"  pcstack:     ${toStackString($pcstack)}\n" +
      s"  exception:   ${$exception}\n" +
      s"  yield:       ${$yield}\n" +
      s"  result:      ${$result}\n" +
      s"  refstackptr: ${$refstackptr}\n" +
      s"  refstack:    ${toStackString($refstack)}\n" +
      s"  valstackptr: ${$valstackptr}\n" +
      s"  valstack:    ${toStackString($valstack)}\n" +
      s">"
    }
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
    def $call(): Instance[T, R]
    def $push(c: Instance[T, R]): Unit
    override def toString = s"Coroutine._0@${System.identityHashCode(this)}"
  }

  abstract class _1[A0, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0): R
    def $call(a0: A0): Instance[T, R]
    def $push(c: Instance[T, R], a0: A0): Unit
    override def toString = s"Coroutine._1@${System.identityHashCode(this)}"
  }

  abstract class _2[A0, A1, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0, a1: A1): R
    def $call(a0: A0, a1: A1): Instance[T, R]
    def $push(c: Instance[T, R], a0: A0, a1: A1): Unit
    override def toString = s"Coroutine._2@${System.identityHashCode(this)}"
  }

  abstract class _3[A0, A1, A2, @specialized T, R] extends Coroutine[T, R] {
    def apply(a0: A0, a1: A1, a2: A2): R
    def $call(a0: A0, a1: A1, a2: A2): Instance[T, R]
    def $push(c: Instance[T, R], a0: A0, a1: A1, a2: A2): Unit
    override def toString = s"Coroutine._3@${System.identityHashCode(this)}"
  }
}
