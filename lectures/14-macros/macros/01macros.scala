/*
 * # Macros
 * We will first discuss a bit what macros are, in particular in Scala, and then how they can be helpful for DSLs.
 */

import scala.reflect.macros.Context

object Macros {
  /*
   Macros are programs that can be invoked in user programs, similarly to functions, but which
   - are executed at compile-time, that is, at runtime of the compiler
   - manipulate the program, and more in general part of the compiler state.

   Here's a first example:
   */

  def plus1Macro(x: Int) = macro plus1Macro_impl
  def plus1Macro_impl(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr(q"$x + 1")
  }

  /*
   * Note that the macro does not perform addition itself - rather, it takes an
   * expression x and produces an expression which contains x. What's the
   * difference with `plus1` below?
   */
  def plus1(x: Int) = x + 1




  /* Answer:
   *
   *
   * plus1Macro generates the result of inlining plus1. Here it does not make
   * much of a difference, because usually inlining is done by the language
   * implementation, but in some particular cases it can be helpful.
   *
   * A more useful example:
   */
  def trace(x: Any): Unit = macro trace_impl
  def trace_impl(c: Context)(x: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    //Wrong!
    //c.Expr(q"""println(s"The value of ${show(x)} is ${x}")""")
    //Not implemented!
    //c.Expr(q"""println(s${s"The value of ${show(x.tree)} is $${x}"})""")
    //Simpler:
    c.Expr(q"""println("The value of %s is %s" format (${show(x.tree)}, $x))""")
  }
}
