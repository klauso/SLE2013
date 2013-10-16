
/* Software Language Engineering Course 
 * University of Marburg
 * Programming Languages and Software Engineering Group
 */
 
 /* These notes are in part based on the book "Programming in Scala"
  * by Martin Odersky et al */
  
 /* This is a brief Scala tutorial. Download Scala at www.scala-lang.org.
  * Start the scala interpreter by typing "scala" on the command prompt.
  * You can load the definitions in this file by the ":l" command.
  */

// =====================================  
// Evaluating expressions and statements
// =====================================
  
// Expressions are evaluated right away
1+2 // yields res0: Int = 3  

// Statements can also be executed

println("Hi") // prints "Hi" 

// ====================
// Definining Variables
// ====================

// Immutable variables are declared with the keyword "val"
val msg = "Hi"
val msg2 = msg + " there"

// It is not possible to reassign a val

// msg = "Hello" yields an error

// You can define the same val again, which shadows the previous definition.

val msg = "Hello"

// If a mutable variable is required you have to use "var".

var x = 5
x = x + 1

// In this course, we will seldomly use "var" and discourage its use.


// ====================
// Defining Functions
// ====================

// Here is the definition of a simple function

def max(x: Int, y: Int) : Int = {
  if (x > y) x else y
}  

// Once a function is defined, it can be called by name.

max(3,4) // yields 4

// We will often use the assert command to automate tests like this one.
// If an assertion fails, an error is raised.

assert(max(3,4) == 4)

// Curly braces can be omitted if the body is just a single expression.
// The return type can also in most cases be omitted and is then
// inferred by the compiler.
def max(x: Int, y: Int) = if (x > y) x else y


// Here is a function with no parameters which returns no interesting value

def sayHello() = println(msg)

// For uniformity, all statements also return a value. If there is no interesting
// value to return, the value () of type Unit is returned. Hence the definition
// above could also be written as

def sayHello() : Unit = println(msg)

assert(sayHello() == () )

// =====
// Loops
// =====

// While loops in Scala are like in most other imperative languages.
// However, their use in Scala is discouraged. In most cases, better solutions
// exist. That said, here is a simple example:

def fact(n: Int) : Int = {
  var res : Int = 1
  var i = 1
  while (i <= n) { 
    res = res * i
	i += 1
  }	
  res
}  

assert(fact(5) == 120)


// =====
// Lists 
// =====

// Here is how you can create an (immutable) list

val oneTwoThree = List(1,2,3)

// Here is another way to create the same list

val oneTwoThree = 1 :: 2 :: 3 :: Nil

// The first version calls a certain constructor of the List class.
// The second one is syntactic sugar for a sequence of method calls,
// namely this one:

val oneTwoThree = Nil.::(3).::(2).::(1)

// An imperative traversal of a list can be done with foreach

oneTwoThree.foreach(x => println(x))

// or, if you like it even shorter

oneTwoThree.foreach(println)

// The foreach function expects a function as parameter, and the => arrow is one
// way to create such functions. More about that later.

// Yet another way to iterate over the collection is to use a for-expression.

for (x <- oneTwoThree) println(x)

// for-expressions are a very powerful and generic language to express queries and
// traversals on collections. More about that later.

// The type List is _generic_. This means that it expects a type as parameter before it
// becomes a proper type. Type arguments are applied using rectangular brackets [] like so:

def head_plus_seven(list_of_ints : List[Int]) = list_of_ints.head + 7
assert( head_plus_seven(oneTwoThree) == 8)

// Type arguments can also be applied to function/method/constructor calls.

val listOfFloats = List[Float](1,2,3)


// The signature of the groupBy function is:
// def groupBy[K](f: (A) ⇒ K): Map[K, List[A]]
// Hence the function expects a type argument.
List("Hi","Hello","Greetings").groupBy[Char](x => x.head)

// Often, but not always, these type arguments can be inferred by the compiler.
// Instead of the above, we could also write
List("Hi","Hello","Greetings").groupBy(x => x.head)

// =========
// Functions
// =========

// First-class functions are constructed like this:

val avg = (x : Float, y: Float) => (x+y)/2
assert(avg(5,7) == 6.0)

// The types of the function arguments can be omitted 
// if they can be inferred from the context

assert( oneTwoThree.filter( x => x > 2) == List(3))

// If the arguments are only used once, they do not need to be named
// and be denoted by an underscore _ with the following alternative syntax:

assert( oneTwoThree.filter( _ > 2) == List(3))

// It is also possible to write multi argument functions that way.
// The left-most _ occurence is interpreted as the first arg, the second one as the
// second, and so forth.

// Example:
assert(oneTwoThree.reduce((x,y) => x+y) == 6)
// can also be written as
assert(oneTwoThree.reduce( _ + _) == 6)


// ======
// Tuples
// ======

// Tuples are constructed with (...,...,...) notation
// They are destructed with ._N notation.

val mypair = (5,"hi")
assert(mypair._2 == "hi")

// Tuples can be nested and are not restricted to pairs (up to 22 positions)
val longtuple = (3,"x",(4,true),false)


// ====
// Sets
// ====

val mySet = Set("Hi", "there")
val otherSet = Set("there", "Hi", "there")
assert(mySet == otherSet)
assert( Set(1,2,3).union(Set(2,3,4)) == Set(1,2,3,4))

// ====
// Maps
// ====

val german2english = Map("Hallo" -> "Hello", "Vorlesung" -> "Lecture")
assert(german2english("Hallo") == "Hello")

// ========================================
// Scala collections - immutable vs mutable
// ========================================

// There exist three kinds of collection types in Scala:
// Immutable collections in scala.collection.immutable._
// Mutable collections in scala.collection.mutable._
// Interfaces that abstract over mutability/immutability in scala.collection._

// Here is an example how these different APIs can be used together.

import scala.collection._

def groupListBy[A,K](l: Seq[A], f: A => K): Map[K, Seq[A]] = {
  import mutable._
  val m = mutable.Map.empty[K, mutable.MutableList[A]]
  for (elem <- l) {
    val key = f(elem)
    val lst = m.getOrElseUpdate(key, mutable.MutableList.empty[A])
    lst += elem
  }
  m
}
// Note: Usually a function like this would be realized by using so-called
// "Builder" objects for lists, rather than mutable.MutableList

groupListBy(List("Hi","Hello","Greetings"), (x:String) => x.head) 

groupListBy[String,Char](List("Hi","Hello","Greetings"), _.head) 


// Homework assignments
// ====================

// 1. Watch the following video:
// http://www.youtube.com/watch?v=_ahvzDzKdB0
// Write a brief summary (250 words max.) of the main points of the talk.
//
// 2. Write a Scala function "combineLists" which takes two lists and 
// composes them element-wise by means of a function, which is also a 
// parameter of the function.
// Example:
// combineLists(List(1,2,3), List(4,5,6), _+_) yields List(5,7,9)
// Make the function as generic as possible by using type parameters.


// Send your homework to kos@informatik.uni-marburg.de . Please choose the following
// subject title of your email: SLE01 - Growing a Language
// The homework must be submitted by October 21, 23:59.
//
// Please note that 80% of all homework assignments must be submitted
// if you want to take the exam.

