package org.coroutines



import org.scalatest._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{ reflectiveCalls, postfixOps }
import scala.util.Success



object AsyncAwaitTest {
  class Cell[+T] {
    var x: T @uncheckedVariance = _
  }

  object ToughTypeObject {
    class Inner

    def m2 = async(coroutine { () =>
      val y = await { Future[List[_]] { Nil } }
      val z = await { Future[Inner] { new Inner } }
      (y, z)
    })
  }

  // Doubly defined for ToughTypeObject
  def await[R]: Future[R] ~~> ((Future[R], Cell[R]), R) =
    coroutine { (f: Future[R]) =>
      val cell = new Cell[R]
      yieldval((f, cell))
      cell.x
    }

  // Doubly defined for ToughTypeObject
  def async[Y, R](body: ~~~>[(Future[Y], Cell[Y]), R]): Future[R] = {
    val c = call(body())
    val p = Promise[R]
    def loop() {
      if (!c.resume) p.success(c.result)
      else {
        val (future, cell) = c.value
        for (x <- future) {
          cell.x = x
          loop()
        }
      }
    }
    Future { loop() }
    p.future
  }
}


class IntWrapper(val value: String) extends AnyVal {
  def plusStr = Future.successful(value + "!")
}


class ParamWrapper[T](val value: T) extends AnyVal


class PrivateWrapper private (private val value: String) extends AnyVal


object PrivateWrapper {
  def Instance = new PrivateWrapper("")
}


class AsyncAwaitTest extends FunSuite with Matchers {
  def await[R]: Future[R] ~~> ((Future[R], AsyncAwaitTest.Cell[R]), R) =
    coroutine { (f: Future[R]) =>
      val cell = new AsyncAwaitTest.Cell[R]
      yieldval((f, cell))
      cell.x
    }

  def async[Y, R](body: ~~~>[(Future[Y], AsyncAwaitTest.Cell[Y]), R]): Future[R] = {
    val c = call(body())
    val p = Promise[R]
    def loop() {
      if (!c.resume) p.success(c.result)
      else {
        val (future, cell) = c.value
        for (x <- future) {
          cell.x = x
          loop()
        }
      }
    }
    Future { loop() }
    p.future
  }

  // Source: https://git.io/vrHtj
  test("propagates tough types") {
    val fut = org.coroutines.AsyncAwaitTest.ToughTypeObject.m2
    val result: (List[_], org.coroutines.AsyncAwaitTest.ToughTypeObject.Inner) =
      Await.result(fut, 2 seconds)
    assert(result._1 == Nil)
  }

  // Source: https://git.io/vr7H9
  test("pattern matching function") {
    val c = async(coroutine { () =>
      await(Future(1))
      val a = await(Future(1))
      val f = { case x => x + a }: Function[Int, Int]
      await(Future(f(2)))
    })
    val res = Await.result(c, 2 seconds)
    assert(res == 3)
  }

  // Source: https://git.io/vr7HA
  test("existential bind 1") {
    def m(a: Any) = async(coroutine { () =>
      a match {
        case s: Seq[_] =>
          val x = s.size
          var ss = s
          ss = s
          await(Future(x))
      }
    })
    val res = Await.result(m(Nil), 2 seconds)
    assert(res == 0)
  }

  // Source: https://git.io/vr7Qm
  test("existential bind 2") {
    def conjure[T]: T = null.asInstanceOf[T]

    def m1 = AsyncAwaitTest.async(coroutine { () =>
      val p: List[Option[_]] = conjure[List[Option[_]]]
      AsyncAwaitTest.await(Future(1))
    })

    def m2 = AsyncAwaitTest.async(coroutine { () =>
      AsyncAwaitTest.await(Future[List[_]](Nil))
    })
  }

  // Source: https://git.io/vr7Fx
  test("existential if/else") {
    trait Container[+A]
    case class ContainerImpl[A](value: A) extends Container[A]
    def foo: Future[Container[_]] = AsyncAwaitTest.async(coroutine { () =>
      val a: Any = List(1)
      if (true) {
        val buf: Seq[_] = List(1)
        val foo = AsyncAwaitTest.await(Future(5))
        val e0 = buf(0)
        ContainerImpl(e0)
      } else ???
    })
    foo
  }

  // Source: https://git.io/vr7ba
  test("ticket 63 in scala/async") {
    object SomeExecutionContext extends ExecutionContext {
      def reportFailure(t: Throwable): Unit = ???
      def execute(runnable: Runnable): Unit = ???
    }

    trait FunDep[W, S, R] {
      def method(w: W, s: S): Future[R]
    }

    object FunDep {
      implicit def `Something to do with List`[W, S, R]
        (implicit funDep: FunDep[W, S, R]) =
        new FunDep[W, List[S], W] {
          def method(w: W, l: List[S]) = AsyncAwaitTest.async(coroutine { () =>
            val it = l.iterator
            while (it.hasNext) {
              AsyncAwaitTest.await(Future(funDep.method(w, it.next()))(SomeExecutionContext))
            }
            w
          })
        }
    }
  }

  // Source: https://git.io/vr7bX
  test("ticket 66 in scala/async") {
    val e = new Exception()
    val f: Future[Nothing] = Future.failed(e)
    val f1 = AsyncAwaitTest.async(coroutine { () =>
      AsyncAwaitTest.await(Future(f))
    })
    try {
      Await.result(f1, 5.seconds)
    } catch {
      case `e` =>
    }
  }

  // Source: https://git.io/vr7Nf
  test("ticket 83 in scala/async-- using value class") {
    val f = AsyncAwaitTest.async(coroutine { () =>
      val uid = new IntWrapper("foo")
      AsyncAwaitTest.await(Future(Future(uid)))
    })
    val outer = Await.result(f, 5.seconds)
    val inner = Await.result(outer, 5 seconds)
    assert(inner == new IntWrapper("foo"))
  }

  // Source: https://git.io/vr7Nk
  test("ticket 86 in scala/async-- using matched value class") {
    def doAThing(param: IntWrapper) = Future(None)

    val fut = AsyncAwaitTest.async(coroutine { () =>
      Option(new IntWrapper("value!")) match {
        case Some(valueHolder) =>
          AsyncAwaitTest.await(Future(doAThing(valueHolder)))
        case None =>
          None
      }
    })

    val result = Await.result(fut, 5 seconds)
    assert(result.asInstanceOf[Future[IntWrapper]].value == Some(Success(None)))
  }

  // Source: https://git.io/vr7NZ
  // NOTE: Need to look at this test's implementation. Might be done incorrectly.
  test("ticket 86 in scala/async-- using matched parameterized value class") {
    def doAThing(param: ParamWrapper[String]) = Future(None)

    val fut = AsyncAwaitTest.async(coroutine { () =>
      Option(new ParamWrapper("value!")) match {
        case Some(valueHolder) =>
          AsyncAwaitTest.await(Future(doAThing(valueHolder)))
        case None =>
          None
      }
    })

    val result = Await.result(fut, 5 seconds)
    assert(result.asInstanceOf[Future[ParamWrapper[String]]].value ==
      Some(Success(None)))
  }

  // Source: https://git.io/vr7NW
  test("ticket 86 in scala/async-- using private value class") {
    def doAThing(param: PrivateWrapper) = Future(None)

    val fut = AsyncAwaitTest.async(coroutine { () => 
      Option(PrivateWrapper.Instance) match {
        case Some(valueHolder) =>
          AsyncAwaitTest.await(doAThing(valueHolder))
        case None =>
          None
      }
    })

    val result = Await.result(fut, 5 seconds)
    assert(result == None)
  }

  // Source: https://git.io/vr7N8
  test("await of abstract type") {
    def combine[A](a1: A, a2: A): A = a1

    def combineAsync[A](a1: Future[A], a2: Future[A]) =
      async(coroutine { () =>
        combine(await(Future(a1)), await(Future(a2)))
      })

    val fut = combineAsync(Future(1), Future(2))

    val outer = Await.result(fut, 5 seconds)
    val inner = Await.result(outer, 5 seconds)
    assert(inner == 1)
  }

  // Source: https://git.io/vrFp5
  test("match as expression 1") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      val x = "" match {
        case _ => AsyncAwaitTest.await(Future(1)) + 1
      }
      x
    })
    val result = Await.result(c, 5 seconds)
    assert(result == 2)
  }

  // Source: https://git.io/vrFhh
  test("match as expression 2") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      val x = "" match {
        case "" if false => await(Future(1)) + 1
        case _           => 2 + await(Future(1))
      }
      val y = x
      "" match {
        case _ => await(Future(y)) + 100
      }
    })
    val result = Await.result(c, 5 seconds)
    assert(result == 103)
  }

  // Source: https://git.io/vrFj3
  test("nested await as bare expression") {
    val c = async(coroutine { () =>
      await(Future(await(Future("")).isEmpty))
    })
    val result = Await.result(c, 5 seconds)
    assert(result == true)
  }

  // Source: https://git.io/vrAnM
  test("nested await in block") {
    val c = async(coroutine { () =>
      ()
      await(Future(await(Future("")).isEmpty))
    })
    val result = Await.result(c, 5 seconds)
    assert(result == true)
  }

  // Source: https://git.io/vrhTe
  test("named and default arguments respect evaluation order") {
    var i = 0
    def next() = {
      i += 1;
      i
    }
    def foo(a: Int = next(), b: Int = next()) = (a, b)
    val c1 = async(coroutine { () =>
      foo(b = await(Future(next())))
    })
    assert(Await.result(c1, 5 seconds) == (2, 1))
    i = 0
    val c2 = async(coroutine { () =>
      foo(a = await(Future(next())))
    })
    assert(Await.result(c2, 5 seconds) == (1, 2))
  }

  // Source: https://git.io/vrhTT
  test("repeated params 1") {
    var i = 0
    def foo(a: Int, b: Int*) = b.toList
    def id(i: Int) = i
    val c = async(coroutine { () =>
      foo(await(Future(0)), id(1), id(2), id(3), await(Future(4)))
    })
    assert(Await.result(c, 5 seconds) == List(1, 2, 3, 4))
  }

  // Source: https://git.io/vrhTY
  test("repeated params 2") {
    var i = 0
    def foo(a: Int, b: Int*) = b.toList
    def id(i: Int) = i
    val c = async(coroutine { () =>
      foo(await(Future(0)), List(id(1), id(2), id(3)): _*)
    })
    assert(Await.result(c, 5 seconds) == List(1, 2, 3))
  }

  // Source: https://git.io/vrhT0
  test("await in typed") {
    val c = async(coroutine { () =>
      (("msg: " + await(Future(0))): String).toString
    })
    assert(Await.result(c, 5 seconds) == "msg: 0")
  }

  // Source: https://git.io/vrhTz
  test("await in assign") {
    val c = async(coroutine { () =>
      var x = 0
      x = await(Future(1))
      x
    })
    assert(Await.result(c, 5 seconds) == 1)
  }

  // Source: https://git.io/vrhTr
  test("case body must be typed as unit") {
    val Up = 1
    val Down = 2
    val sign = async(coroutine { () =>
      await(Future(1)) match {
        case Up   => 1.0
        case Down => -1.0
      }
    })
    assert(Await.result(sign, 5 seconds) == 1.0)
  }
}
