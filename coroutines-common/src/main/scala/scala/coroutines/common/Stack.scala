package org.coroutines.common



import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



object Stack {
  def init[T](stack: Array[T], size: Int): Unit = macro initMacro[T]

  def initMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree, size: c.Tree): c.Tree = {
    import c.universe._

    val tpe = implicitly[c.WeakTypeTag[T]]
    if (size == q"-1") q"" else q"""
      if ($stack == null) $stack = new Array[$tpe]($size)
    """
  }

  def copy[T](src: Array[T], dest: Array[T]): Unit = macro copyMacro[T]

  def copyMacro[T: c.WeakTypeTag](c: Context)(src: c.Tree, dest: c.Tree): c.Tree = {
    import c.universe._

    val q"$srcpath.${srcname: TermName}" = src
    val srcptrname = TermName(s"${srcname}ptr")
    val srcptr = q"$srcpath.$srcptrname"
    val q"$destpath.${destname: TermName}" = dest
    val destptrname = TermName(s"${destname}ptr")
    val destptr = q"$destpath.$destptrname"
    val tpe = implicitly[WeakTypeTag[T]]

    q"""
      $destptr = $srcptr
      if ($src != null) {
        $dest = new Array[$tpe]($src.length)
        java.lang.System.arraycopy($src, 0, $dest, 0, $srcptr)
      }
    """
  }

  def push[T](stack: Array[T], x: T, size: Int): Unit = macro pushMacro[T]

  def pushMacro[T: c.WeakTypeTag](c: Context)(
    stack: c.Tree, x: c.Tree, size: c.Tree
  ): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val tpe = implicitly[WeakTypeTag[T]]
    q"""
      org.coroutines.common.Stack.init[$tpe]($stack, $size)
      if ($stackptr >= $stack.length) {
        val nstack = new Array[$tpe]($stack.length * 2)
        java.lang.System.arraycopy($stack, 0, nstack, 0, $stack.length)
        $stack = nstack
      }
      $stack($stackptr) = $x
      $stackptr += 1
    """
  }

  def bulkPush[T](stack: Array[T], n: Int, size: Int): Unit = macro bulkPushMacro[T]

  def bulkPushMacro[T: c.WeakTypeTag](c: Context)(
    stack: c.Tree, n: c.Tree, size: c.Tree
  ): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val tpe = implicitly[WeakTypeTag[T]]
    val valnme = TermName(c.freshName())
    q"""
      org.coroutines.common.Stack.init[$tpe]($stack, $size)
      $stackptr += $n
      while ($stackptr >= $stack.length) {
        val nstack = new Array[$tpe]($stack.length * 2)
        java.lang.System.arraycopy($stack, 0, nstack, 0, $stack.length)
        $stack = nstack
      }
    """
  }

  def pop[T](stack: Array[T]): T = macro popMacro[T]

  def popMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val tpe = implicitly[WeakTypeTag[T]]
    val valnme = TermName(c.freshName())
    q"""
      $stackptr -= 1
      val $valnme = $stack($stackptr)
      $stack($stackptr) = null.asInstanceOf[$tpe]
      $valnme
    """
  }

  def bulkPop[T](stack: Array[T], n: Int): Unit = macro bulkPopMacro[T]

  def bulkPopMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree, n: c.Tree): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val tpe = implicitly[WeakTypeTag[T]]
    val valnme = TermName(c.freshName())
    q"""
      $stackptr -= $n
    """
  }

  def top[T](stack: Array[T]): T = macro topMacro[T]

  def topMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    q"""
      $stack($stackptr - 1)
    """
  }

  def get[T](stack: Array[T], n: Int): T = macro getMacro[T]

  def getMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree, n: c.Tree): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val valnme = TermName(c.freshName())
    q"""
      $stack($stackptr - 1 - $n)
    """
  }

  def set[T](stack: Array[T], n: Int, x: T): Unit = macro setMacro[T]

  def setMacro[T: c.WeakTypeTag](c: Context)(
    stack: c.Tree, n: c.Tree, x: c.Tree
  ): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val valnme = TermName(c.freshName())
    q"""
      $stack($stackptr - 1 - $n) = $x
    """
  }

  def update[T](stack: Array[T], x: T): T = macro updateMacro[T]

  def updateMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree, x: c.Tree): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    val valnme = TermName(c.freshName())
    q"""
      val $valnme = $stack($stackptr - 1)
      $stack($stackptr - 1) = $x
      $valnme
    """
  }

  def isEmpty[T](stack: Array[T]): Boolean = macro isEmptyMacro[T]

  def isEmptyMacro[T: c.WeakTypeTag](c: Context)(stack: c.Tree): c.Tree = {
    import c.universe._

    val q"$path.${name: TermName}" = stack
    val stackptrname = TermName(s"${name}ptr")
    val stackptr = q"$path.$stackptrname"
    q"""
      $stackptr <= 0
    """
  }


}
