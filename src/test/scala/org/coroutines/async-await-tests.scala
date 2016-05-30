package org.coroutines


import org.scalatest._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



object AsyncAwaitTest {
  class Cell[+T] {
    var x: T @uncheckedVariance = _
  }

  def await[R]: Future[R] ~~> ((Future[R], Cell[R]), R) =
    coroutine { (f: Future[R]) =>
      val cell = new Cell[R]
      yieldval((f, cell))
      cell.x
    }

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

  object ToughTypeObject {
    class Inner

    def m2 = async(coroutine { () =>
      val y = await { Future[List[_]] { Nil } }
      val z = await { Future[Inner] { new Inner } }
      (y, z)
    })
  }
}


class IntWrapper(val value: String) extends AnyVal


class AsyncAwaitTest extends FunSuite with Matchers {
  // Source: https://git.io/vrHtj
  test("propagates tough types") {
    val fut = org.coroutines.AsyncAwaitTest.ToughTypeObject.m2
    val res: (List[_], org.coroutines.AsyncAwaitTest.ToughTypeObject.Inner) =
      Await.result(fut, 2 seconds)
    assert(res._1 == Nil)
  }

  // Source: https://git.io/vrHmG
  // NOTE: Currently fails compilation
  /*
  test("pattern matching partial function") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      AsyncAwaitTest.await(Future(1))
      val a = AsyncAwaitTest.await(Future(1))
      val f = { case x => x + a }: PartialFunction[Int, Int]
      AsyncAwaitTest.await(Future(f(2)))
    })
    val res = Await.result(c, 2 seconds)
    assert(res == 3)
  }
  */

  // Source: https://git.io/vr79k
  // NOTE: Currently fails compilation
  /*
  test("pattern matching partial function nested") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      AsyncAwaitTest.await(Future(1))
      val neg1 = -1
      val a = AsyncAwaitTest.await(Future(1))
      val f = {case x => ({case x => neg1 * x}:
          PartialFunction[Int, Int])(x + a)}: PartialFunction[Int, Int]
      AsyncAwaitTest.await(Future(f(2)))
    })
    val res = Await.result(c, 2 seconds)
    assert(res == -3)
  }
  */

  // Source: https://git.io/vr7H9
  test("pattern matching function") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      AsyncAwaitTest.await(Future(1))
      val a = AsyncAwaitTest.await(Future(1))
      val f = { case x => x + a }: Function[Int, Int]
      AsyncAwaitTest.await(Future(f(2)))
    })
    val res = Await.result(c, 2 seconds)
    assert(res == 3)
  }

  // Source: https://git.io/vr7HA
  test("existential bind 1") {
    def m(a: Any) = AsyncAwaitTest.async(coroutine { () =>
      a match {
        case s: Seq[_] =>
          val x = s.size
          var ss = s
          ss = s
          AsyncAwaitTest.await(Future(x))
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

  // Source: https://git.io/vr7FE
  // NOTE: Currently fails compilation
  /**
  test("singleton type") {
    class A { class B }
    AsyncAwaitTest.async(coroutine { () =>
      val a = new A
      def foo(b: a.B) = 0
      AsyncAwaitTest.await(Future(foo(new a.B)))
    })
  }
  */
  

  // Source: https://git.io/vr7F6
  // NOTE: Currently fails compilation
  /**
  test("existential match") {
    trait Container[+A]
    case class ContainerImpl[A](value: A) extends Container[A]
    def foo: Future[Container[_]] = AsyncAwaitTest.async(coroutine { () =>
      val a: Any = List(1)
      a match {
        case buf: Seq[_] =>
          val foo = AsyncAwaitTest.await(Future(5))
          val e0 = buf(0)
          ContainerImpl(e0)
      }
    })
    foo
  }
  */

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

  // Source: https://git.io/vr7bJ
  // NOTE: Currently fails compilation
  /**
  test("nested method with inconsistency") {
    import language.{reflectiveCalls, postfixOps}

    class Foo[A]

    object Bippy {

      import ExecutionContext.Implicits.global

      def bar(f: => Unit): Unit = f

      def quux: Future[String] = ???

      def foo = AsyncAwaitTest.async(coroutine { () =>
        def r[A](m: Foo[A])(n: A) = {
          bar {
            locally(m)
            locally(n)
            identity[A] _
          }
        }

        AsyncAwaitTest.await(quux)

        r(new Foo[String])("")
      })
    }
    Bippy
  }
  */

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
  /** NOTE: Ignoring this test until I find the correct implementation of
    * `IntWrapper`.
  test("ticket 83 in scala/async-- using value class") {
    val f = AsyncAwaitTest.async(coroutine { () =>
      val uid = new IntWrapper("foo")
      AsyncAwaitTest.await(Future(Future(uid)))
    })
    val outer = Await.result(f, 5.seconds)
    val inner = Await.result(outer, 5 seconds)
    assert(inner == new IntWrapper("foo"))
  }
  */


  // Source: https://git.io/vr7NJ
  /** NOTE: Ignoring this test until I find the correct implementation of
    * `IntWrapper`.
  test("ticket 86 in scala/async-- using nested value class") {
    val f = AsyncAwaitTest.async(coroutine { () =>
      val a = Future.successful(new IntWrapper("42"))
      AsyncAwaitTest.await(AsyncAwaitTest.await(a).plusStr)
    })
    val outer = Await.result(f, 5 seconds)
    val inner = Await.result(outer, 5 seconds)
    assert(inner == "42!")
  }
  */

  // Source: https://git.io/vr7Nk
  /** NOTE: Ignoring this test until I find the correct implementation of
    * `IntWrapper`.
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

    val result = Await.result(fut, 5.seconds)
    result mustBe None
  }
  */

  // Source: https://git.io/vr7NZ
  /** NOTE: Ignoring this test until I find the correct implementation of
    * `ParamWrapper`.
  test("ticket 86 in scala/async-- using matched parameterized value class") {
  }
  */

  // Source: https://git.io/vr7NW
  /** NOTE: Ignoring this test until I find the correct implementation of
    * `ParamWrapper`.
  test("ticket 86 in scala/async-- using private value class") {

  }
  */

  // Source: https://git.io/vr7N8
  test("await of abstract type") {
    def combine[A](a1: A, a2: A): A = a1

    def combineAsync[A](a1: Future[A], a2: Future[A]) =
      AsyncAwaitTest.async(coroutine { () =>
        combine(AsyncAwaitTest.await(Future(a1)), AsyncAwaitTest.await(Future(a2)))
      })

    val fut = combineAsync(Future(1), Future(2))

    val outer = Await.result(fut, 5 seconds)
    val inner = Await.result(outer, 5 seconds)
    assert(inner == 1)
  }

  // Source: https://git.io/vr7NB
  /** NOTE: Ignoring this test until I find the correct implementation of
    * `IntWrapper`.
  test("ticket 106 in scala/async-- value class") {

  }
  */
}
