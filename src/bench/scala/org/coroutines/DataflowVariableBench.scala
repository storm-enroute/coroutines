package org.coroutines



import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic._
import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.annotation.tailrec
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure



class DataflowVariableBench extends JBench.OfflineReport {
  override def defaultConfig = Context(
    exec.minWarmupRuns -> 80,
    exec.maxWarmupRuns -> 120,
    exec.benchRuns -> 36,
    exec.independentSamples -> 4,
    verbose -> true
  )

  val sizes = Gen.range("size")(50000, 250000, 50000)

  class FutureDataflowVar[T] {
    private val p = Promise[T]()
    def apply(cont: T => Unit): Unit = p.future.onComplete {
      case Success(x) => cont(x)
      case Failure(t) => throw t
    }
    def :=(x: T): Unit = p.success(x)
  }

  class FutureDataflowStream[T](val head: T) {
    val tail = new FutureDataflowVar[FutureDataflowStream[T]]
  }

  // @gen("sizes")
  // @benchmark("coroutines.dataflow.producer-consumer")
  // @curve("future")
  // def futureProducerConsumer(sz: Int) = {
  //   val root = new FutureDataflowVar[FutureDataflowStream[String]]()
  //   def producer(left: Int, tail: FutureDataflowVar[FutureDataflowStream[String]]) {
  //     val s = new FutureDataflowStream("")
  //     tail := s
  //     if (left > 0) producer(left - 1, s.tail)
  //   }
  //   val done = Promise[Boolean]()
  //   def consumer(left: Int, tail: FutureDataflowVar[FutureDataflowStream[String]]) {
  //     if (left == 0) done.success(true)
  //     else tail(s => consumer(left - 1, s.tail))
  //   }

  //   val p = Future {
  //     producer(sz, root)
  //   }
  //   val c = Future {
  //     consumer(sz, root)
  //   }
  //   Await.result(done.future, 10.seconds)
  // }

  @gen("sizes")
  @benchmark("coroutines.dataflow.bounded-producer-consumer")
  @curve("future")
  def futureBoundedProducerConsumer(sz: Int) = {
    val root = new FutureDataflowVar[FutureDataflowStream[String]]()
    val startTokens = new FutureDataflowVar[FutureDataflowStream[String]]()
    def producer(
      left: Int,
      tail: FutureDataflowVar[FutureDataflowStream[String]],
      tokenTail: FutureDataflowVar[FutureDataflowStream[String]]
    ) {
      val s = new FutureDataflowStream("")
      tail := s
      if (left > 0) tokenTail(toks => producer(left - 1, s.tail, toks.tail))
    }
    val done = Promise[Boolean]()
    def consumer(
      left: Int,
      tail: FutureDataflowVar[FutureDataflowStream[String]],
      tokenTail: FutureDataflowVar[FutureDataflowStream[String]]
    ) {
      if (left == 0) done.success(true)
      else {
        val toks = new FutureDataflowStream("")
        tokenTail := toks
        tail(s => consumer(left - 1, s.tail, toks.tail))
      }
    }

    def fill(t: FutureDataflowVar[FutureDataflowStream[String]], left: Int):
      FutureDataflowVar[FutureDataflowStream[String]] = {
      val s = new FutureDataflowStream("")
      t := s
      if (left > 0) fill(s.tail, left - 1)
      else s.tail
    }
    val tokens = fill(startTokens, 50)
    val p = Future {
      producer(sz, root, startTokens)
    }
    val c = Future {
      consumer(sz, root, tokens)
    }
    Await.result(done.future, 10.seconds)
  }

  @transient lazy val forkJoinPool = new ForkJoinPool

  def task[T](body: ~~~>[DataflowVar[T], Unit]) {
    val c = call(body())
    schedule(c)
  }

  def schedule[T](c: DataflowVar[T] <~> Unit) {
    forkJoinPool.execute(new Runnable {
      @tailrec final def run() {
        if (c.resume) {
          val dvar = c.value
          @tailrec def subscribe(): Boolean = {
            val state = dvar.get
            if (state.isInstanceOf[List[_]]) {
              if (dvar.compareAndSet(state, c :: state.asInstanceOf[List[_]])) true
              else subscribe()
            } else false
          }
          if (!subscribe()) run()
        }
      }
    })
  }

  class DataflowVar[T] extends AtomicReference[AnyRef](Nil) {
    val apply = coroutine { () =>
      if (this.get.isInstanceOf[List[_]]) yieldval(this)
      this.get.asInstanceOf[T]
    }
    @tailrec final def :=(x: T): Unit = {
      val state = this.get
      if (state.isInstanceOf[List[_]]) {
        if (this.compareAndSet(state, x.asInstanceOf[AnyRef])) {
          var cs = state.asInstanceOf[List[DataflowVar[T] <~> Unit]]
          while (cs != Nil) {
            schedule(cs.head)
            cs = cs.tail
          }
        } else this := x
      } else {
        sys.error("Already assigned!")
      }
    }
    override def toString = s"DataflowVar${this.get}"
  }

  class DataflowStream[T](val head: T) {
    val tail = new DataflowVar[DataflowStream[T]]
  }

  @gen("sizes")
  @benchmark("coroutines.dataflow.producer-consumer")
  @curve("coroutine")
  def coroutineProducerConsumer(sz: Int) = {
    val root = new DataflowVar[DataflowStream[String]]
    val tokens = new DataflowVar[DataflowStream[String]]
    val done = Promise[Boolean]()
    val producer = coroutine { () =>
      var left = sz
      var tail = root
      var tokenTail = tokens
      while (left > 0) {
        tail := new DataflowStream("")
        tail = tail.apply().tail
        tokenTail = tokenTail.apply().tail
        left -= 1
      }
    }
    val startTokens = {
      var t = tokens
      var left = 50
      while (left > 0) {
        val s = new DataflowStream("")
        t := s
        t = s.tail
        left -= 1
      }
      t
    }
    val consumer = coroutine { () =>
      var left = sz
      var tail = root
      var tokenTail = startTokens
      while (left > 0) {
        tail = tail.apply().tail
        val s = new DataflowStream("")
        tokenTail := s
        tokenTail = s.tail
        left -= 1
      }
      done.success(true)
      ()
    }

    task(producer)
    task(consumer)

    Await.result(done.future, 10.seconds)
  }
}
