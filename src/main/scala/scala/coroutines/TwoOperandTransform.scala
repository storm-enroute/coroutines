package scala.coroutines



import scala.collection._
import scala.coroutines.common._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context



/** Transforms the coroutine body into a two operand assignment form with restricted
 *  control flow that contains only try-catch statements, while loops, do-while loops,
 *  if-statements, value and variable declarations, nested blocks and function calls.
 *
 *  Two operand transform also populates the `Table` object with local variable info.
 */
trait TwoOperandTransform[C <: Context] {
  val c: C

  import c.universe._
}
