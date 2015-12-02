package scala.coroutines.common



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



private[coroutines] class ByTreeTyper[C <: Context](val c: C)(val treeValue: Any) {
  import c.universe._
  private val tree = treeValue.asInstanceOf[Tree]
  private val treeMapping = mutable.Map[Tree, Tree]()
  private val traverser = new TraverserUtil[c.type](c)
  val untypedTree = c.untypecheck(tree)
  traverser.traverseByShape(untypedTree, tree)((t, pt) => treeMapping(t) = pt)

  def typeOf(t: Tree) = if (treeMapping.contains(t)) treeMapping(t).tpe else t.tpe
}
