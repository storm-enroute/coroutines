package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class TreeIteratorBench extends JBench.OfflineReport {
  override def defaultConfig = Context(
    exec.minWarmupRuns -> 40,
    exec.maxWarmupRuns -> 80,
    exec.benchRuns -> 30,
    exec.independentSamples -> 5,
    verbose -> false
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

  var treeEnumerator: Coroutine._1[Tree, Int, Unit] = null

  /* max int */

  @gen("trees")
  @benchmark("coroutines.tree-iterator.max")
  @curve("coroutine")
  def coroutineMax(tree: Tree) {
    var max = Int.MinValue
    treeEnumerator = coroutine { (t: Tree) =>
      t match {
        case n: Node =>
          if (n.left != Empty) treeEnumerator(n.left)
          yieldval(n.x)
          if (n.right != Empty) treeEnumerator(n.right)
        case Empty =>
      }
    }
    val c = call(treeEnumerator(tree))
    while (c.pull) {
      val x = c.value
      if (x > max) max = x
    }
  }

  @gen("trees")
  @benchmark("coroutines.tree-iterator.max")
  @curve("iterator")
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

  @gen("trees")
  @benchmark("coroutines.tree-iterator.to-array")
  @curve("coroutine")
  def coroutineToArray(tree: Tree) {
    val a = new IntArray
    treeEnumerator = coroutine { (t: Tree) =>
      t match {
        case n: Node =>
          if (n.left != Empty) treeEnumerator(n.left)
          yieldval(n.x)
          if (n.right != Empty) treeEnumerator(n.right)
        case Empty =>
      }
    }
    val c = call(treeEnumerator(tree))
    while (c.pull) {
      val x = c.value
      a.add(x)
    }
  }

  @gen("trees")
  @benchmark("coroutines.tree-iterator.to-array")
  @curve("iterator")
  def iteratorToArray(tree: Tree) {
    val a = new IntArray
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
    val a = new IntArray
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

  /* tests */

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

}
