package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class GraphIteratorBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 50,
    exec.maxWarmupRuns -> 100,
    exec.benchRuns -> 36,
    exec.independentSamples -> 4,
    verbose -> true
  )

  val SPARSE_DEG = 3

  val DENSE_DEG = 16

  val sparseSizes = Gen.range("size")(50000, 250000, 50000)

  val denseSizes = Gen.range("size")(50000, 250000, 50000)

  def graphs(sizes: Gen[Int], density: Int) = for (sz <- sizes) yield {
    var totalNeighbours = 0
    val nodes = mutable.Buffer[Node[String]]()
    var g = new Graph[String]()
    var n = g.add("root")
    for (i <- 0 until sz) {
      n = n.add(i.toString)

      val neighbours =
        if (nodes.length > 0)
          (0 until density).map(j => math.abs(i ^ (i + j)) % nodes.length).distinct
        else Nil
      totalNeighbours += neighbours.length
      for (index <- neighbours) {
        n.neighbours += nodes(index)
      }

      nodes += n
    }

    g
  }

  val sparseGraphs = graphs(sparseSizes, SPARSE_DEG)

  val denseGraphs = graphs(denseSizes, DENSE_DEG)

  var dfsEnumerator: Coroutine._1[Graph[String], String, Unit] = null

  def initDfsEnumerator() {
    def addNeighbours(
      stack: mutable.ArrayBuffer[Node[String]], visited: Array[Boolean], n: Node[String]
    ) {
      var i = 0
      while (i < n.neighbours.length) {
        val m = n.neighbours(i)
        if (!visited(m.index)) {
          stack += m 
          visited(m.index) = true
        }
        i += 1
      }
    }
    dfsEnumerator = coroutine { (g: Graph[String]) =>
      val visited = new Array[Boolean](g.indexCount)
      val stack = mutable.ArrayBuffer[Node[String]]()
      for (n <- g.roots) {
        stack += n
        visited(n.index) = true
      }
      while (stack.length > 0) {
        val n = stack.remove(stack.length - 1)
        addNeighbours(stack, visited, n)
        yieldval(n.elem)
      }
    }
  }

  var bfsEnumerator: Coroutine._1[Graph[String], String, Unit] = null

  def initBfsEnumerator() {
    def addNeighbours(
      queue: mutable.Queue[Node[String]], visited: Array[Boolean], n: Node[String]
    ) {
      var i = 0
      while (i < n.neighbours.length) {
        val m = n.neighbours(i)
        if (!visited(m.index)) {
          queue.enqueue(m)
          visited(m.index) = true
        }
        i += 1
      }
    }
    bfsEnumerator = coroutine { (g: Graph[String]) =>
      val visited = new Array[Boolean](g.indexCount)
      val queue = mutable.Queue[Node[String]]()
      for (n <- g.roots) {
        queue += n
        visited(n.index) = true
      }
      while (queue.length > 0) {
        val n = queue.dequeue()
        addNeighbours(queue, visited, n)
        yieldval(n.elem)
      }
    }
  }

  /* to buffer */

  @gen("sparseGraphs")
  @benchmark("coroutines.sparse-graph-dfs.to-buffer")
  @curve("coroutine")
  def coroutineSparseDfs(g: Graph[String]) = {
    val buffer = mutable.Buffer[String]()
    initDfsEnumerator()
    val c = call(dfsEnumerator(g))
    while (c.resume) {
      val s = c.value
      buffer += s
    }
    buffer
  }

  @gen("sparseGraphs")
  @benchmark("coroutines.sparse-graph-dfs.to-buffer")
  @curve("iterator")
  def iteratorSparseDfs(g: Graph[String]) = {
    val buffer = mutable.Buffer[String]()
    val i = new GraphDfsIterator(g)
    while (i.hasNext) {
      val s = i.next()
      buffer += s
    }
    buffer
  }

  @gen("denseGraphs")
  @benchmark("coroutines.dense-graph.dfs.to-buffer")
  @curve("coroutine")
  def coroutineDenseDfs(g: Graph[String]) = {
    val buffer = mutable.Buffer[String]()
    initDfsEnumerator()
    val c = call(dfsEnumerator(g))
    while (c.resume) {
      val s = c.value
      buffer += s
    }
    buffer
  }

  @gen("denseGraphs")
  @benchmark("coroutines.dense-graph.dfs.to-buffer")
  @curve("iterator")
  def iteratorDenseDfs(g: Graph[String]) = {
    val buffer = mutable.Buffer[String]()
    val i = new GraphDfsIterator(g)
    while (i.hasNext) {
      val s = i.next()
      buffer += s
    }
    buffer
  }

  @gen("sparseGraphs")
  @benchmark("coroutines.sparse-graph.bfs.to-buffer")
  @curve("coroutine")
  def coroutineSparseBfs(g: Graph[String]) = {
    val buffer = mutable.Buffer[String]()
    initBfsEnumerator()
    val c = call(bfsEnumerator(g))
    while (c.resume) {
      val s = c.value
      buffer += s
    }
    buffer
  }

  @gen("sparseGraphs")
  @benchmark("coroutines.sparse-graph.bfs.to-buffer")
  @curve("iterator")
  def iteratorSparseBfs(g: Graph[String]) = {
    val buffer = mutable.Buffer[String]()
    val i = new GraphBfsIterator(g)
    while (i.hasNext) {
      val s = i.next()
      buffer += s
    }
    buffer
  }

}
