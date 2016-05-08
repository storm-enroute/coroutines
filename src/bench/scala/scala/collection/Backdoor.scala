package scala.collection



import org.coroutines._



object Backdoor {

  type RBTree[T] = immutable.RedBlackTree.Tree[T, Unit]

  def redBlack[T](set: immutable.TreeSet[T]): RBTree[T] = {
    val f = set.getClass.getDeclaredField("tree")
    f.setAccessible(true)
    f.get(set).asInstanceOf[immutable.RedBlackTree.Tree[T, Unit]]
  }

  val redBlackEnumeratorInlined: Coroutine._1[RBTree[String], String, Unit] =
    coroutine { (tree: RBTree[String]) =>
      if (tree.left != null) {
        if (tree.left.left != null) redBlackEnumerator(tree.left.left)
        yieldval(tree.left.key)
        if (tree.left.right != null) redBlackEnumerator(tree.left.right)
      }
      yieldval(tree.key)
      if (tree.right != null) {
        if (tree.right.left != null) redBlackEnumerator(tree.right.left)
        yieldval(tree.right.key)
        if (tree.right.right != null) redBlackEnumerator(tree.right.right)
      }
    }

  val redBlackEnumerator: Coroutine._1[RBTree[String], String, Unit] =
    coroutine { (tree: RBTree[String]) =>
      if (tree.left != null) redBlackEnumerator(tree.left)
      yieldval(tree.key)
      if (tree.right != null) redBlackEnumerator(tree.right)
    }

  def hashSet[T](set: mutable.HashSet[T]): Array[AnyRef] = {
    val f = set.getClass.getDeclaredField("table")
    f.setAccessible(true)
    f.get(set).asInstanceOf[Array[AnyRef]]
  }

  val hashSetEnumerator: Coroutine._1[Array[AnyRef], String, Unit] =
    coroutine { (table: Array[AnyRef]) =>
      var i = 0
      while (i < table.length) {
        val x = table(i)
        if (x != null) yieldval(x.asInstanceOf[String])
        i += 1
      }
    }

}
