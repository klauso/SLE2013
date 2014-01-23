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

  def plus1Macro(x: Int): Int = macro plus1Macro_impl
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
    //Base version:
    //c.Expr(q"""println("The value of %s is %s" format (${show(x)}, $x))""")

    //Question: what's wrong if we write instead:
    //c.Expr(q"""println(${"The value of %s is %s" format (show(x), x)})""")

    //Refined version, removing the "Expr[Nothing]" from the output.
    c.Expr(q"""println("The value of %s is %s" format (${show(x.tree)}, $x))""")
  }
  //Now, look at and run 02macrosClient.scala.

  def traceRaw(x: Any): Unit = macro traceRaw_impl
  def traceRaw_impl(c: Context)(x: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(q"""println("The value of %s is %s" format (${showRaw(x.tree)}, $x))""")
  }

  /*
   To repeat:

   Macros are programs that can be invoked in user programs, similarly to functions, but which
   - are executed at compile-time, that is, at runtime of the compiler
   - manipulate the program, and more in general part of the compiler state.

   Moreover:
   Programs which manipulate other programs are called *metaprograms*.
   Macros are metaprograms!

   Macros are run on code before it is executed. In compiled languages, that
   means at compile-time; in interpreted languages, this typically means when
   the code is loaded, but code loading and execution are often less strictly
   separated. We will ignore interpreted languages.

   We have seen so-called "def macros", which are called like functions: a call
   to such a macro is an expression.

   However, sometimes we want to generate declarations rather than expressions.
   A function call cannot manipulate declarations, since functions are executed
   at runtime but declarations must be known at compile-time. This gives rise to
   other "macro flavors", which we'll however not discuss today.

   Since macros act at compile-time, sometimes they can be used in contexts where
   functions are not allowed because that context contains something that must be
   known at compile-time (declarations of types/methods/etc.).

   # When are macros executed, during compilation?
   In general, there are several possible choices here.
   - In C, macros are executed after lexing the input. This is very simple to implement, but is very hard to use.

   Typical example (using C syntax):

     #define TWICE(x) x * 2
     TWICE(x + 1) // expands to:
     x + 1 * 2 //while we'd want:
     (x + 1) * 2

   Not recommended.
   - Alternatively, it is possible to run macros after parsing, that is, after
   building ASTs. This would fix the above problem, since the macro manipulates
   a tree. However, after parsing names have not been resolved yet.

   Typical example (using again C syntax):

     #define SWAP(typ, x, y) { typ temp = x; x = y; y = temp; }
     SWAP(int, x, temp) //expands to:
     { int temp = x; x = temp; temp = temp; } //but we'd instead like:
     { int _temp = x; x = temp; temp = _temp; }

   - To solve the above problem, we can do macro expansion after name resolution, so that we can automatically fix the problem.
   This is done for instance in Scheme.

   - However, in Scala, name resolution is interleaved with typechecking, also
   because of overloading. Moreover, it is often useful when analyzing trees to
   have them already typechecked. So def macros (the flavor we have seen) are run
   after typechecking. This also allows giving a signature to Scala macros.
   */

  /*
   trace_impl simply uses the show() method to analyze the tree and transform it to a string.
   However, very often we need to actually inspect the tree.
   */

  def inspectComprehension[T](e: List[T]): List[T] = macro inspectComprehension_impl[T]
  def inspectComprehension_impl[T: c.WeakTypeTag](c: Context)(e: c.Expr[List[T]]): c.Expr[List[T]] ={
    import c.universe._
    c.Expr[List[T]](e.tree match {
      //Don't work:
      case q"$l.map($f).map($g)" => q"$l.map($f andThen $g)"
      case q"$l.map($f)($cbf1).map($g)($cbf2)" => q"$l.map($f andThen $g)"

      //Works:
      case q"$l.map[$t1, $t2]($f)($cbf1).map[$t3, $t4]($g)($cbf2)" => q"$l.map($f andThen $g)"
      case q"$l.map[$t1, $t2]($f)($cbf1).flatMap[$t3, $t4]($g)($cbf2)" => q"$l.flatMap($f andThen $g)"
      case q"$l.flatMap[$t1, $t2]($f)($cbf1).map[$t3, $t4]($g)($cbf2)" => q"$l.flatMap($f andThen (_ map $g))"
      case t => t
    })
  }
}

/*
 References:
 - The Reflection Guide: http://docs.scala-lang.org/overviews/reflection/overview.html
 - The Macros Guide: http://docs.scala-lang.org/overviews/macros/overview.html
 - Eugene Burmako. Scala Macros: Let Our Powers Combine! On How Rich Syntax and Static Types Work with Metaprogramming. In Proceedings of Scala Workshop '13. ACM, 2013. http://scalamacros.org/news/2013/04/22/let-our-powers-combine.html
 */
