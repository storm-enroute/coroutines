package org.coroutines






class TreeIteratorBench extends JBench.OfflineReport {

  val size = Gen.range("size")(500, 2500, 500)

  override def defaultConfig = Context(
    exec.minWarmupRuns -> 20,
    exec.maxWarmupRuns -> 40,
    exec.benchRuns -> 32,
    exec.independentSamples -> 4
  )

  sealed trait Tree
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  val treeIterator = coroutine { (t: Tree) =>
    t match {
      case Tree(left, right) =>
        treeIterator(left)
        treeIterator(right)
      case Leaf(x) =>
        yieldval(x)
    }
  }

  @gen("sizes")
  @benchmark("coroutines.tree-iterator")
  @curve("Coroutine")
  def coroutineApply(matrix: Matrix) {
  }

}
