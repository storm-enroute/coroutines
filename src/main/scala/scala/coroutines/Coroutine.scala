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



class Coroutine[@specialized T] {
  import Coroutine._
  var $costackptr = 0
  var $costack: Array[Blueprint[T]] = new Array[Blueprint[T]](INITIAL_COSTACK_SIZE)
  var $pcstackptr = 0
  var $pcstack = new Array[Short](INITIAL_COSTACK_SIZE)
  var $refstackptr = 0
  var $refstack: Array[AnyRef] = _
  var $valstackptr = 0
  var $valstack: Array[Int] = _
  var $target: Coroutine[T] = null
  var $result: T = null.asInstanceOf[T]

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


object Coroutine {
  private[coroutines] val INITIAL_COSTACK_SIZE = 4

  @tailrec
  private[coroutines] final def enter[T](c: Coroutine[T]): T = {
    val cd = Stack.top(c.$costack)
    cd.$enter(c)
    if (c.$target ne null) {
      val nc = c.$target
      c.$target = null
      enter(nc)
    } else {
      val res = c.$result
      c.$result = null.asInstanceOf[T]
      res
    }
  }

  trait BlueprintMarker[@specialized T]

  trait Blueprint[@specialized T] extends BlueprintMarker[T] {
    def $enter(c: Coroutine[T]): Unit
    def $assignresult(c: Coroutine[T], v: T): Unit = c.$result = v
    def $returnvalue(c: Coroutine[T], v: T): Unit
    def $ep0(c: Coroutine[T]): Unit = {}
    def $ep1(c: Coroutine[T]): Unit = {}
    def $ep2(c: Coroutine[T]): Unit = {}
    def $ep3(c: Coroutine[T]): Unit = {}
    def $ep4(c: Coroutine[T]): Unit = {}
    def $ep5(c: Coroutine[T]): Unit = {}
    def $ep6(c: Coroutine[T]): Unit = {}
    def $ep7(c: Coroutine[T]): Unit = {}
    def $ep8(c: Coroutine[T]): Unit = {}
    def $ep9(c: Coroutine[T]): Unit = {}
    def $ep10(c: Coroutine[T]): Unit = {}
    def $ep11(c: Coroutine[T]): Unit = {}
    def $ep12(c: Coroutine[T]): Unit = {}
    def $ep13(c: Coroutine[T]): Unit = {}
    def $ep14(c: Coroutine[T]): Unit = {}
    def $ep15(c: Coroutine[T]): Unit = {}
    def $ep16(c: Coroutine[T]): Unit = {}
    def $ep17(c: Coroutine[T]): Unit = {}
    def $ep18(c: Coroutine[T]): Unit = {}
    def $ep19(c: Coroutine[T]): Unit = {}
    def $ep20(c: Coroutine[T]): Unit = {}
    def $ep21(c: Coroutine[T]): Unit = {}
    def $ep22(c: Coroutine[T]): Unit = {}
    def $ep23(c: Coroutine[T]): Unit = {}
    def $ep24(c: Coroutine[T]): Unit = {}
    def $ep25(c: Coroutine[T]): Unit = {}
    def $ep26(c: Coroutine[T]): Unit = {}
    def $ep27(c: Coroutine[T]): Unit = {}
    def $ep28(c: Coroutine[T]): Unit = {}
    def $ep29(c: Coroutine[T]): Unit = {}
  }

  def synthesize(c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).synthesize(f)
  }

  def call[T: c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    new Synthesizer[c.type](c).call(f)
  }

  abstract class _0[@specialized T] extends Blueprint[T] {
    def apply(): T
    def $call(): Coroutine[T]
    def $push(c: Coroutine[T]): Unit
  }

  trait _1[A0, @specialized T] extends Blueprint[T] {
    def apply(a0: A0): T
    def $call(a0: A0): Coroutine[T]
    def $push(c: Coroutine[T], a0: A0): Unit
  }

  abstract class _2[A0, A1, @specialized T] extends Blueprint[T] {
    def apply(a0: A0, a1: A1): T
    def $call(a0: A0, a1: A1): Coroutine[T]
    def $push(c: Coroutine[T], a0: A0, a1: A1): Unit
  }

  abstract class _3[A0, A1, A2, @specialized T] extends Blueprint[T] {
    def apply(a0: A0, a1: A1, a2: A2): T
    def $call(a0: A0, a1: A1, a2: A2): Coroutine[T]
    def $push(c: Coroutine[T], a0: A0, a1: A1, a2: A2): Unit
  }
}
