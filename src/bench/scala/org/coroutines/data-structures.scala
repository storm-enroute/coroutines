package org.coroutines



import scala.collection._



class IntArray {
  private var array = new Array[Int](8)
  private var size = 0
  def length = size
  def apply(idx: Int) = {
    assert(idx >= 0 && idx < size)
    array(idx)
  }
  def add(x: Int) = {
    if (size == array.length) {
      val narray = new Array[Int](size * 2)
      System.arraycopy(array, 0, narray, 0, size)
      array = narray
    }
    array(size) = x
    size += 1
  }
  def push(x: Int) = add(x)
  def top = array(size - 1)
  def pop() = {
    val x = array(size - 1)
    size -= 1
    x
  }
}


class Graph[T] {
  var indexCount = 0
  val roots = mutable.Buffer[Node[T]]()

  def add(elem: T): Node[T] = {
    val n = new Node(this, indexCount, elem)
    indexCount += 1
    roots += n
    n
  }
}


class Node[T](val graph: Graph[T], val index: Int, val elem: T) {
  val neighbours = mutable.Buffer[Node[T]]()

  def add(elem: T): Node[T] = {
    val n = new Node(graph, graph.indexCount, elem)
    graph.indexCount += 1
    neighbours += n
    n
  }
}


class GraphDfsIterator[T](val graph: Graph[T]) {
  val visited = new Array[Boolean](graph.indexCount)
  val stack = mutable.ArrayBuffer[Node[T]]()
  def enq(n: Node[T]) = stack += n
  def deq(): Node[T] = stack.remove(stack.length - 1)
  for (n <- graph.roots) {
    enq(n)
    visited(n.index) = true
  }
  def hasNext = stack.length > 0
  def next(): T = {
    val n = deq()
    var i = 0
    while (i < n.neighbours.length) {
      val m = n.neighbours(i)
      if (!visited(m.index)) {
        enq(m)
        visited(m.index) = true
      }
      i += 1
    }
    n.elem
  }
}


class GraphBfsIterator[T](val graph: Graph[T]) {
  val visited = new Array[Boolean](graph.indexCount)
  val queue = mutable.Queue[Node[T]]()
  def enq(n: Node[T]) = queue.enqueue(n)
  def deq(): Node[T] = queue.dequeue()
  for (n <- graph.roots) {
    enq(n)
    visited(n.index) = true
  }
  def hasNext = queue.length > 0
  def next(): T = {
    val n = deq()
    var i = 0
    while (i < n.neighbours.length) {
      val m = n.neighbours(i)
      if (!visited(m.index)) {
        enq(m)
        visited(m.index) = true
      }
      i += 1
    }
    n.elem
  }
}
