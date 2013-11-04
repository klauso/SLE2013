/* Software Language Engineering Course 
 * University of Marburg
 * Programming Languages and Software Engineering Group
 */
 
 /* These notes are in part based on the paper 
    "Type Classes as Implicits and Objects" by Oliveira et al (ECOOP'12)
	http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf
  */

/* 
Scala allows the type-driven automatic selection of values with the
"implicit" keyword. There are two kinds of implicits in Scala: Implicit
conversions and implicit parameters. We will discuss implicit parameters first. */

// ===================
// Implicit Parameters
// ===================

/* A method call may omit the final argument list (i.e.
the arguments in the last pair of round brackets) if that argument list
is annotated with the "implicit" keyword.

Here is an example: */

def max[T](x1: T, x2: T)(implicit ordering: Ordering[T]) =
  if (ordering.lt(x1,x2)) x2 else x1

// For comparison, the same example without the implicit keyword  
def max_wo_implicit[T](x1: T, x2: T)(ordering: Ordering[T]) =
  if (ordering.lt(x1,x2)) x2 else x1
  
// Let's consider some data type that can be ordered.  
case class Student(name: String, matNr: Int)

// Here is an implementation of the Ordering type for Student.
// It is marked as an implicit object. Hence it will be selected
// automatically when a function that requires an implicit of
// the type Ordering[Student] is called.

implicit object StudentOrder extends Ordering[Student] {
  def compare(s1: Student, s2: Student) = s1.name.compare(s2.name)
}
 
// For instance, with
val s1 = Student("Jones",54321)
val s2 = Student("Smith",12345)
// we can now call
val s3 = max(s1,s2)
// instead of the more explicit
val s4 = max_wo_implicit(s1,s2)(StudentOrder)

// Even if a parameter is marked as implicit, we can still pass an explicit
// value that will potentially override the implicit one.
// For instance, there are different feasible orderings for students, including this one

object StudentMatNrOrder extends Ordering[Student] {
  def compare(s1: Student, s2: Student) = s1.matNr.compare(s2.matNr)
}  
  
val s5 = max(s1,s2)(StudentMatNrOrder)
  
/* The alternative to the design above is to make class Student implement 
the "Ordered" interface. The advantage of the design above is that it is
more modular. We do not need to preplan the design of that interface
when implementing Student. Rather, the class can be extended retroactively,
without modifying its code. We can also see that it is simple to implement
multiple orderings, which would be difficult with the idea of implementing
the Ordered interface.

Usually the drawback of the design above is that we have to pass all those
extra objects. But with implicit parameters we do not have to do that anymore.
But we still can, if needed. */

/* Not only objects can be marked as implicit values, but also "val" and "var" 
definitions. Here is an example:*/

import java.io.PrintStream

implicit val out = System.out

def log(msg: String)(implicit o: PrintStream) = o.println(msg)

log("Argh! this should not happen!")
log("Argh! this should not happen!")(System.err)

// It is also possible to have multiple implicit arguments:

def log(msg: String)(implicit o: PrintStream, prefix: String) = 
   o.println("["+prefix+"]"+msg)

implicit val defaultPrefix = "Warning"

log("Argh! this should not happen!")

// We can of course still pass the implicit arguments explicitly
log("Argh! this should not happen!")(System.err, "error")

// It would be nice if one could supply only a subset of the 
// implicit parameters explicitly and provide "wildcards" for the rest.
// This can be achieved with the following small idiom:

def ?[T](implicit w: T) : T = w

// Now we can either pass only the first implicit explicitly:

log("Argh! this should not happen!")(System.err, ?)

log("Argh! this should not happen!")(?, "error")

// The reason is that the calls to ? will turn into ?(out) and 
// ?(defaultPrefix), respectively.

/* When looking for an implicit value of type T, the compiler will
consider implicit value definitions as well as implicit arguments 
of type T that are in scope locally where the implicit value is required.

Here is an example to illustrate the scoping rules. */

trait Monoid[A] {
  def op(x: A, y: A) : A
  def zero : A
}
def accumulate[A](l: List[A])(implicit m: Monoid[A]) : A = 
  l.foldLeft(m.zero)((x,y) => m.op(x,y))

object A {
  implicit object sumMonoid extends Monoid[Int] {
    def op(x: Int, y: Int) = x+y
    def zero = 0
  }
  
  // sumMonoid is choosen here because it is the only implicit value 
  // of the correct type which is in scope
  def sum(l: List[Int]) = accumulate(l) 
}

object B {
  implicit object prodMonoid extends Monoid[Int] {
    def op(x: Int, y: Int) = x*y
    def zero = 1
  }
  // prodMonoid is choosen here because it is the only implicit value 
  // of the correct type which is in scope
  def prod(l: List[Int]) = accumulate(l)
}
  
def test = { 
  import A._
  import B._
  val l = List(1,2,3,4,5)
  println(sum(l)) // prints 15
  println(prod(l)) // prints 120
  println(accumulate(l)(prodMonoid)) // prints 120
  // println(accumulate(l)) //error: ambiguous implicit values
}

// A common use case of implicits is to retroactively extend
// a type by some feature. In this case, we will typically
// have arguments of a type "T" and an implicit argument of
// type Foo[T], where Foo is the interface that describes
// the added features. An example of that is the Ordering
// example from above:
def max[T](x1: T, x2: T)(implicit ordering: Ordering[T]) =
  if (ordering.lt(x1,x2)) x2 else x1

// Scala provides a shorthand notation called "context bounds"
// for this situation. The method header can be rewritten as:
  
// def max[T: Ordering](x1: T, x2: T) = ...

// One peculiarity of that approach is that the Ordering object
// has no name within max, but it is available as implicit
// value within its definition. This means that we can 
// get access to it via our ? trick again, if needed.
 
def max[T: Ordering](x1: T, x2: T) =
  if (?[Ordering[T]].lt(x1,x2)) x2 else x1  

  
// ====================  
// Implicit Conversions
// ====================  


/* Implicit values that are functions (either defined as ordinary
functions or as methods via "def") have a special semantics: They
can be used as _views_. A view is an implicit value f that has 
a functional type S => T. A call to f is inserted in situations where
a value of type T is required but a value of type S is given.
More specifically, views are applied in two situations:

i) if an expression e is of type T and T does not conform to the 
expression's expected type
ii) in a selection e.m with e of type T, if the selection m does not
denote a member (method, field) of T.

Scala demands that implicit conversions must be enabled explicitly
by the following import: */
import scala.language.implicitConversions

/* A common usage of views is the "Pimp-my-library" pattern, see
http://www.artima.com/weblogs/viewpost.jsp?thread=179766

For instance, scala.Predef contains the following definition:

implicit def intWrapper(x: Int)         = new runtime.RichInt(x)

This means that all functions that are defined in RichInt can be
used on integers, such as the method "until:
*/

val onetofive = 1 until 5

// Hence the "pimp-my-library" pattern allows to simulate the
// extension of a type without actually modifying it. It is
// used extensively in the Scala standard library.

/* Implicits methods can return a new object of a different type, but
they don't need to (the body of the method can be arbitrary code).

Since the "pimp my library" pattern is such a common case, Scala
provides some syntactic sugar for it, namely in the form of 
_implicit classes_. An implicit class is a class definition with 
the modifier "implicit". 

An implicit class must have a primary constructor with exactly 
one argument in its first parameter list. It may also include an 
additional implicit parameter list.

Implicit classes are desugared into the "pimp my library" pattern.
Example: instead of defining the method intWrapper as above, the
class RichInt could also be declared as:

implicit class RichInt(n: Int) ...

Similar to context bounds for implicit parameters, there is 
the notion of _view bounds_ for views. 

For instance, we can write

def f[A <% B](a: A) = a.bMethod

which is a shorthand for writing this:

def f[A](a: A)(implicit ev: A => B) = a.bMethod

(where ev is a fresh name that is not available within the body of f)

Example: Instead of using the "Ordering" trait to order Students,
we could also use the "Ordered" trait and define a corresponding view:
*/

implicit def student2ordered(s: Student) = 
  new Ordered[Student] { def compare(other: Student) = s.name.compare(other.name) }
                                   
def max_viewbound[T <% Ordered[T]](x1: T, x2: T) =
  if (x1 < x2) x2 else x1

  
/* Implicit parameters and implicit views can also be used together for useful
purposes. For instance, given an ordering for values of type T, we can define
an order for lists of Ts: */
  
class ListOrd[T](o: Ordering[T]) extends Ordering[List[T]] {
  def compare(l1: List[T], l2: List[T]) = (l1,l2) match {
    case (x::xs, y::ys) => if (o.equiv(x,y)) compare(xs,ys) else o.compare(x,y)
	case (_,Nil) => -1
	case (Nil,_) => 1
  }
}

// Now we can define a view to ListOrd that takes the element ordering
// as implicit parameter.
implicit def ord2listord[T](implicit o: Ordering[T]) = new ListOrd(o)


// And now we can happily treat lists of students as ordered things:

val l1 = List(Student("a",5), Student("b",7))
val l2 = List(Student("c",1), Student("x",44))
val res = max(l1,l2)

// And also lists of any other ordered thing (such as integers)

val res2 = max(List(1,2,3), List(4,5,6))

// Lists of ordered things are ordered, hence lists of lists of ordered things, too:

val l3 = List(l1,l2)
val l4 = List(l2,l1)
val res3 = max(l3,l4)

// Note that the implicit argument in the last call is:

val res4 = max(l3,l4)(ord2listord(ord2listord(StudentOrder)))