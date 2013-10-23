// ==========
// Extractors
// ==========

/* Scala supports pattern matching as in most functional languages,
   but it does it in an "object-oriented" way:
   1) Pattern matching in Scala is user-extensible The same
   objects can be decomposed according to multiple patterns.
   2) Pattern matching in Scala does not necessarily expose
   data representations (which is one difference between OO and FP).
   
   Scala supports pattern matching with two constructs: Extractors
   and "match" clauses. Extractors are objects (often but not necessarily 
   singleton or companion objects) with a function "unapply" that takes
   a single argument and returns Option[T] for some tuple type T. The purpose
   of unapply is to take its argument apart and return those parts in
   a tuple. 
   
   Let's look at an example: */
   
object Email {
  def unapply(str: String) : Option[(String,String)] = {
      val parts = str split "@"
	  if (parts.length == 2) Some( parts(0),parts(1)) else None
  }
}

assert(("foo@bar.com" match {case Email(user,domain) => user}) == "foo")
 

// ====================
// Variance Annotations
// ====================

/* 
If S is a subtype of T, should U[S] be a subtype of U[T], for a type constructor U?
The answer depends. If we take an immutable collection, then yes: */
val v: List[Object] = List("a","b","c")
/* We call a type like List for which this property holds _covariant_ in its type parameter.
However, if we take a mutable collection, then no: */

// val q: Array[Object] = Array("a","b","c")  -- this produces a static type error

/* There are also some type constructors for which the reverse relation holds:
If S is a subtype of T, then U[S] is a supertype of U[T]. We call such type
constructors _contravariant_ in their type argument.
An example of this is the first type argument of the function type constructor: */

val f = (x : Object) => x.toString // f has type Object => String
val g : String => String = f       // => is is contravariant in its first argument
val h : String => Object = g       // => is covariant in its second argument


/* Scala allows to control the variance of a type parameter via _variance annotations_. 
   "+" after a type parameter means that it is a covariant type parameter; "-" means
   contravariant and nothing means _invariant_ (which means that there is no
   subtype relation in any direction between U[S] and U[T], even if S is a subtype of T.

   If we make a variance annotation, then the compiler will check that it can never be
   a type problem if U[S] is a subtype of U[T] (or vice versa for contravariance).
   Informally speaking, if we declare T to be covariant, then the type parameter 
   may only appear in method return types or types of immutable fields, but not in return types.
   Contravariant type parameters may only appear in method argument types.
*/

class Cell1[+T](x: T) {
  def get : T  = x // ok because T is covariant
  // def set(t: T) // not ok because T is covariant
}  
val v1 : Cell1[Object] = new Cell1[String]("hi") // ok because T is covariant

class Cell2[-T] {
  // def get : T // not ok because T is contravariant
   def set(t: T) {} // ok because T is contravariant
}  
val v2 : Cell2[String] = new Cell2[Object] // ok because T is contravariant

class Cell3[T](var x: T) {
   def get : T = x //  ok because T is invariant
   def set(t: T) { x=t } // ok because T is invariant
}  
//val v3 : Cell3[String] = new Cell3[Object](v1)  // not ok because T is invariant
// val v4 : Cell3[Object] = new Cell3[String]("hi") // not ok because T is invariant
  
/* The variance mechanism in Scala is different than the wildcards known from Java.
   Java uses so-called _use-site variance_ and Scala _definition-site variance_.
   Use-site variance means that variance is annotated in those places where
   the type variable is instantiated, such as in a variable declaration.
   Definition-site variance means that the variance is declared where the
   type parameter is declared. Which of these mechanisms is preferable is subject
   to intensive debates; generally use-site variance is more expressive (allows
   more programs to type-check) but is harder to use.
   
   Scala also supports a variant of use-site variance by means of so-called 
   existential types, but we will not consider existential types in detail
   at this point. */

   
// ===========
// Type Bounds
// ===========
/* It is possible to constrain the set of possible instantiations of a type parameter
by means of _bounds_. There are two types of bounds: Lower bounds and upper bounds.
Let's start with a small type hierarchy to illustrate what we do. */

class A { val a = 1 }
class B extends A { val b = 2 }
class C extends B { val c = 3 }

// Here is an example of an upper bound: T can be any subtype of B.
// If we contrain the type parameter, then we can make use of the features we know from the bound.
class Nonsense[T <: B](x : T) {
  val q = x.b + x.a
}
val test = new Nonsense[C](new C) // ok
val test = new Nonsense[B](new B) // ok
// val test = Nonsense[A](new A) // type error, bound violated.

// Here is an example of a lower bound: T must be a supertype of B
class MoreNonsense[T >: B] {
  def appendToList(l: List[T]) : List[T] = (new C) :: l
}

// Bounds do not have to be constant; rather, they can depend on the type argument.
// A common case is this (imagine the body to be a snippet within the sort routine).
// Such bounds are sometimes called F-bounds (where F stands for "function").

def sort[T <: Ordered[T]](l: List[T]) = l(1) < l(2)


// ==================
// In-Class Exercise:
// ==================

// Write a class Stack (and possibly other classes) such
// that the following code compiles and runs without errors

/* class Fruit
class Apple extends Fruit
class Orange extends Fruit

val s1 : Stack[Apple] = EmptyStack.push(new Apple)
val s2 : Stack[Orange] = EmptyStack
val f : Fruit = s1.push(new Orange).top
val s4 : Stack[Object] = s1.pop

def size[A](x: Stack[A]) : Int = x match { 
    case EmptyStack => 0
    case Stack(e,r) => 1+size(r) 
}

assert(size(s1)== 1)
*/
	
// ===================	
// Homework Assignment 
// ===================

// Homework is due until October 30, 23:59 via email
// to kos@informatik.uni-marburg.de
	
// Program a class to support immutable binary trees in Scala.
// It should be possible to construct, analyze and decompose
// them in flexible and convenient ways. 
// Use variance annotations and type bounds,
// to make the implementation as generic as possible. Use extractors
// to control pattern matching for your trees.
// Supply a set of test cases (via assert) that demonstrate
// all interesting features of your implementation.
// Try to avoid language features that we have not yet discussed in class.

