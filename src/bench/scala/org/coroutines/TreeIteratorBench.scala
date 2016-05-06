package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class TreeIteratorBench extends JBench.OfflineReport {

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 20,
    exec.maxWarmupRuns -> 40,
    exec.benchRuns -> 32,
    exec.independentSamples -> 4,
    verbose -> true
  )

  sealed trait Tree
  case class Node(x: Int, left: Tree, right: Tree) extends Tree
  case object Empty extends Tree

  class TreeIterator(val tree: Tree) {
    var stack = new Array[Tree](20)
    var stackpos = -1
    var current: Int = _

    def goLeft(tree: Tree) {
      stackpos += 1
      stack(stackpos) = tree
      tree match {
        case Empty =>
        case Node(_, left, _) => goLeft(left)
      }
    }

    goLeft(tree)
    moveToNext()

    def moveToNext() {
      if (stackpos != -1) stack(stackpos) match {
        case Empty =>
          stackpos -= 1
          moveToNext()
        case Node(x, _, right) =>
          stackpos -= 1
          current = x
          goLeft(right)
      }
    }

    def hasNext: Boolean = {
      stackpos != -1
    }
    def next(): Int = {
      val x = current
      moveToNext()
      x
    }
  }

  assert({
    def leaf(x: Int) = Node(x, Empty, Empty)
    val tree = Node(1,
      Node(19, leaf(21), leaf(23)),
      Node(3,
        leaf(11),
        Node(9,
          leaf(5),
          leaf(17))))
    val a = mutable.Buffer[Int]()
    def rec(tree: Tree): Unit = tree match {
      case Empty =>
      case Node(x, l, r) =>
        rec(l)
        a += x
        rec(r)
    }
    rec(tree)
    val b = mutable.Buffer[Int]()
    val it = new TreeIterator(tree)
    while (it.hasNext) b += it.next()
    a == b
  })

  val sizes = Gen.range("size")(50000, 250000, 50000)
  val trees = for (sz <- sizes) yield {
    def gen(sz: Int): Tree = {
      if (sz == 0) Empty
      else {
        val rem = sz - 1
        val left = gen(rem / 2)
        val right = gen(rem - rem / 2)
        Node(sz, left, right)
      }
    }
    gen(sz)
  }

  var treeIterator: Coroutine._1[Tree, Int, Unit] = null

  /* max int */

  @gen("trees")
  @benchmark("coroutines.tree-iterator.max")
  @curve("Coroutine")
  def coroutineMax(tree: Tree) {
    var max = Int.MinValue
    treeIterator = coroutine { (t: Tree) =>
      t match {
        case Node(x, left, right) =>
          treeIterator(left)
          yieldval(x)
          treeIterator(right)
        case Empty =>
      }
    }
    val c = call(treeIterator(tree))
    while (c.pull) {
      val x = c.value
      if (x > max) max = x
    }
  }

  @gen("trees")
  @benchmark("coroutines.tree-iterator.max")
  @curve("Iterator")
  def iteratorMax(tree: Tree) {
    var max = Int.MinValue
    val iter = new TreeIterator(tree)
    while (iter.hasNext) {
      val x = iter.next()
      if (x > max) max = x
    }
  }
 
  @gen("trees")
  @benchmark("coroutines.tree-iterator.max")
  @curve("recursion")
  def recursiveMax(tree: Tree) {
    var max = Int.MinValue
    def recurse(tree: Tree) {
      tree match {
        case Node(x, left, right) =>
          recurse(left)
          if (x > max) max = x
          recurse(right)
        case Empty =>
      }
    }
    recurse(tree)
  }

  /* growing array */

  class GrowingArray {
    private var array = new Array[Int](8)
    private var size = 0
    def add(x: Int) = {
      if (size == array.length) {
        val narray = new Array[Int](size * 2)
        System.arraycopy(array, 0, narray, 0, size)
        array = narray
      }
      array(size) = x
      size += 1
    }
  }

  @gen("trees")
  @benchmark("coroutines.tree-iterator.to-array")
  @curve("Coroutine")
  def coroutineToArray(tree: Tree) {
    val a = new GrowingArray
    treeIterator = coroutine { (t: Tree) =>
      t match {
        case Node(x, left, right) =>
          treeIterator(left)
          yieldval(x)
          treeIterator(right)
        case Empty =>
      }
    }
    val c = call(treeIterator(tree))
    while (c.pull) {
      val x = c.value
      a.add(x)
    }
  }

  @gen("trees")
  @benchmark("coroutines.tree-iterator.to-array")
  @curve("Iterator")
  def iteratorToArray(tree: Tree) {
    val a = new GrowingArray
    val iter = new TreeIterator(tree)
    while (iter.hasNext) {
      val x = iter.next()
      a.add(x)
    }
  }

  @gen("trees")
  @benchmark("coroutines.tree-iterator.to-array")
  @curve("recursion")
  def recursiveToArray(tree: Tree) {
    val a = new GrowingArray
    def recurse(tree: Tree) {
      tree match {
        case Node(x, left, right) =>
          recurse(left)
          a.add(x)
          recurse(right)
        case Empty =>
      }
    }
    recurse(tree)
  }

}
