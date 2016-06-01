package org.coroutines


import org.scalatest._
import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success


object AsyncAwaitTest {
  class Cell[+T] {
    var x: T @uncheckedVariance = _
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

  object ToughTypeObject {
    class Inner

    def m2 = async(coroutine { () =>
      val y = await { Future[List[_]] { Nil } }
      val z = await { Future[Inner] { new Inner } }
      (y, z)
    })
  }
}


class IntWrapper(val value: String) extends AnyVal {
  def plusStr(): IntWrapper = new IntWrapper(value + "!")
}


class ParamWrapper[T](val value: T) extends AnyVal 


private class PrivateWrapper(val value: String) extends AnyVal


private object PrivateWrapper {
  def Instance() = new PrivateWrapper("Thugga")
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

  // Source: https://git.io/vrHmG
  /** NOTE: Currently fails compilation.
  test("pattern matching partial function") {
    val c = async(coroutine { () =>
      await(Future(1))
      val a = await(Future(1))
      val f = { case x => x + a }: PartialFunction[Int, Int]
      await(Future(f(2)))
    })
    val res = Await.result(c, 2 seconds)
    assert(res == 3)
  }
  */

  // Source: https://git.io/vr79k
  /** NOTE: Currently fails compilation.
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

  // Source: https://git.io/vr7FE
  /** NOTE: Currently fails compilation.
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
  /** NOTE: Currently fails compilation.
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
  /** NOTE: Currently fails compilation
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
  test("ticket 83 in scala/async-- using value class") {
    val f = AsyncAwaitTest.async(coroutine { () =>
      val uid = new IntWrapper("foo")
      AsyncAwaitTest.await(Future(Future(uid)))
    })
    val outer = Await.result(f, 5.seconds)
    val inner = Await.result(outer, 5 seconds)
    assert(inner == new IntWrapper("foo"))
  }

  // Source: https://git.io/vr7NJ
  /** NOTE: This test currently fails compilation.
  test("ticket 86 in scala/async-- using nested value class") {
    val f = AsyncAwaitTest.async[Nothing, IntWrapper](coroutine { () =>
      val a = Future.successful(new IntWrapper("42"))
      AsyncAwaitTest.await(Future(AsyncAwaitTest.await(a).plusStr))
    })
    val res = Await.result(f, 5 seconds)
    assert(res == "42!")
  }
  */

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

  // Source: https://git.io/vr7NB
  /** NOTE: Currently fails compilation
  test("ticket 106 in scala/async-- value class") {
    AsyncAwaitTest.async(coroutine { () =>
      "whatever value" match {
        case _ =>
          AsyncAwaitTest.await(Future("whatever return type"))
          new IntWrapper("value case matters")
      }
      "whatever return type"
    })
  }
  */
  

  // Source: https://git.io/vrFQt
  /** NOTE: Currently fails compilation.
  test("Inlining block does not produce duplicate definition") {
    AsyncAwaitTest.async(coroutine { () =>
      val f = 12
      val x = AsyncAwaitTest.await(Future(f))
      {
        type X = Int
        val x: X = 42
        println(x)
      }
      type X = Int
      x: X
    })
  }
  */

  // Source: https://git.io/vrF5X
  /** NOTE: Currently fails compilation.
  test("Inlining block in tail position does not produce duplication definition") {
    val c = AsyncAwaitTest.async(coroutine { () =>
      val f = 12
      val x = AsyncAwaitTest.await(Future(f))
      {
        val x = 42
        x
      }
    })
    val res = Await.result(c, 5 seconds)
    assert(res == 42)
  }
  */

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

  // Source: https://git.io/vrAWm
  // NOTE: Currently fails compilation
  /**
  test("nested await in if") {
    val c: Future[Any] = async(coroutine { () =>
      if ("".isEmpty) {
        await(Future(await(Future("")).isEmpty))
      } else 0
    })
    assert(Await.result(c, 5 seconds) == true)
  }
  */

  // Source: https://git.io/vrAlJ
  /** NOTE: This test currently fails because the future times out. 
   *  Interestingly, the future doesn't time out if `1` is passed as the first
   *  argument to `foo`. 
   */
  test("by-name expressions aren't lifted") {
    def foo(ignored: => Any, b: Int) = b
    val c = async(coroutine { () =>
      await(Future(foo(???, await(Future(1)))))
    })
    val result = Await.result(c, 5 seconds)
    assert(result == 1)
  }

  // Source: https://git.io/vrA0Q
  /** NOTE: Currently fails compilation. If I remove the parentheses after 
   * `next`, but keep the ones after the calls to `next` on the line
   * `foo(next(), await(Future(next())))`,
   *  then there the compiler is able to find `next` and there is no macro
   *  expansion error. However, there is the expected compilation error saying
   *  that "`Int` does not take parameters."
   *  Removing the parentheses to make the line as such:
   *  `foo(next, await(Future(next)))`
   *  creates another macro expansion error.
  test("evaluation order respected") {
    def foo(a: Int, b: Int) = (a, b)
    val c = async(coroutine { () =>
      var i = 0
      def next(): Int = {
        i += 1
        i
      }
      foo(next(), await(Future(next())))
    })
    val result = Await.result(c, 5 seconds)
    assert(result == 1)
  }
  */

  // Source: https://git.io/vrAuv
  /** NOTE: Currently fails compilation. As in the previous test, `get` cannot
   be found.*/
  /**
  test("await in non-primary param section 1") {
    def foo(a0: Int)(b0: Int) = s"a0 = $a0, b0 = $b0"
    val c = async(coroutine {() =>
      var i = 0
      def get = { i += 1; i }
      foo(get)(await(Future(get)))
    })
    assert(Await.result(c, 5 seconds) == "a0 = 1, b0 = 2")
  }
  */
}
