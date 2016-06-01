package org.coroutines



import org.scalameter.api._
import org.scalameter.japi.JBench
import scala.collection._



class TreeIteratorBench extends JBench.OfflineReport {
  override def defaultConfig = Context(
    exec.minWarmupRuns -> 40,
    exec.maxWarmupRuns -> 80,
    exec.benchRuns -> 30,
    exec.independentSamples -> 1,
    verbose -> true
  )

  sealed trait Tree
  case class Node(x: Int, left: Tree, right: Tree) extends Tree
  case object Empty extends Tree

  class TreeIterator(val tree: Tree) {
    var stack = new Array[Tree](30)
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
          stack(stackpos) = null
          stackpos -= 1
          if (stackpos > -1) assert(stack(stackpos) != Empty)
          moveToNext()
        case Node(x, _, right) =>
          stack(stackpos) = null
          stackpos -= 1
          current = x
          goLeft(right)
      }
    }

    def hasNext: Boolean = {
      stackpos != -1
    }
    def next(): Int = {
      if (!hasNext) throw new NoSuchElementException
      val x = current
      moveToNext()
      x
    }
  }

  val sizes = Gen.range("size")(50000, 250000, 50000)

  def genTree(sz: Int): Tree = {
    if (sz == 0) Empty
    else {
      val rem = sz - 1
      val left = genTree(rem / 2)
      val right = genTree(rem - rem / 2)
      Node(sz, left, right)
    }
  }

  val trees = for (sz <- sizes) yield {
    genTree(sz)
  }

  val treePairs = for (sz <- sizes) yield {
    (genTree(sz), genTree(sz))
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

  /* samefringe */

  @volatile var isSame = true

  @gen("treePairs")
  @benchmark("coroutines.tree-iterator.same-fringe")
  @curve("coroutine")
  def coroutineSameFringe(p: (Tree, Tree)) {
    val (t1, t2) = p
    treeEnumerator = coroutine { (t: Tree) =>
      t match {
        case n: Node =>
          if (n.left != Empty) treeEnumerator(n.left)
          yieldval(n.x)
          if (n.right != Empty) treeEnumerator(n.right)
        case Empty =>
      }
    }
    val c1 = call(treeEnumerator(t1))
    val c2 = call(treeEnumerator(t2))
    var same = true
    while (c1.pull && c2.pull) {
      val x = c1.value
      val y = c2.value
      if (x != y) same = false
    }
    isSame = same
  }

  @gen("treePairs")
  @benchmark("coroutines.tree-iterator.same-fringe")
  @curve("iterator")
  def iteratorSameFringe(p: (Tree, Tree)) {
    val (t1, t2) = p
    val iter1 = new TreeIterator(t1)
    val iter2 = new TreeIterator(t2)
    var same = true
    while (iter1.hasNext && iter2.hasNext) {
      val x = iter1.next()
      val y = iter2.next()
      if (x != y) same = false
    }
    if (iter1.hasNext != iter2.hasNext) same = false
    isSame = same
  }

  def treeStream(tree: Tree): Stream[Int] = {
    tree match {
      case Empty => Stream()
      case Node(x, left, right) => treeStream(left) #::: (x #:: treeStream(right))
    }
  }

  @gen("treePairs")
  @benchmark("coroutines.tree-iterator.same-fringe")
  @curve("stream")
  def streamSameFringe(p: (Tree, Tree)) {
    val (t1, t2) = p
    var s1 = treeStream(t1)
    var s2 = treeStream(t2)
    var same = true
    while (s1.nonEmpty && s2.nonEmpty) {
      val x = s1.head
      val y = s2.head
      if (x != y) same = false
      s1 = s1.tail
      s2 = s2.tail
    }
    if (s1.nonEmpty != s2.nonEmpty) same = false
    isSame = same
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
