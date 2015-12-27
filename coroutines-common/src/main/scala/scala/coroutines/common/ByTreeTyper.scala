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

  object typeOf {
    private val augmentedTypes = mutable.Map[Tree, Type]()
    def apply(t: Tree) = {
      if (augmentedTypes.contains(t)) augmentedTypes(t)
      else if (treeMapping.contains(t)) treeMapping(t).tpe
      else t.tpe
    }
    def update(t: Tree, tpe: Type) = augmentedTypes(t) = tpe
  }
}
