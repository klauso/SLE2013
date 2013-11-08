
// ===================
// Homework Assignment
// ===================

/*
Homework is due on Nov 13, 23:59 via email
to pgiarrusso@informatik.uni-marburg.de AND kos@informatik.uni-marburg.de
Please choose the subject title "SLE04"
*/

// Consider the following list and for comprehension:

val x = List(List(1,2), List(3,4))
val res1 = for (i <- x; j <- i if j+1 <= 3) yield 42+j

// Your task is to write a function ListWrapper, such that
// the following code, or code which is at least very similar
// to this one, can be executed:

val y = ListWrapper(x)

val res2 = for (i <- y; j <- i if j+1 <= 3) yield 42+j


// The intended effect of ListWrapper is that the for comprehension
// is reified into an abstract syntax tree. That is, while
// res1 contains the result of the query,
assert(res1 == List(43,44))
// , res2 should contain a representation of the query.
// For instance, it should be possible to ask the representation
// a question like "how many additions show up in the query?".
// Prove that this works by writing a function "numberOfAdds"
// that counts the number of additions in a query:
assert(numberOfAdds(res2) == 2)

// It is not necessary to support more operations than those that show up
// in this particular query. For instance, there is no need to support
// multiplication or a ">=" operation.

// Try to make the representation such that queries that 
// would not type-check on y do not typecheck on x either. For instance
// the following query should be rejected by the type checker:

for (i <- y; j <- i+1 if j+1 <= 3) yield j+42

// This part of the homework is optional, however. We will also accept solutions 
// that are not typed



// ======
// Hints
// ======

// You might want to define a datatype to represent Scala expressions
// of type T, either untyped like so:

sealed abstract class Exp

// or, if you are going for the typed solution, like so:

sealed abstract class Exp[T]

// For literals, you might want to define implicit conversions
// to an adequate representation, such as this one:

implicit class Num(n: Int) extends Exp  // untyped
implicit class Num(n: Int) extends Exp[Int]  // typed

// Make sure that representations of list (values of type Exp[List[T]]) 
// support map, flatMap, and withFilter
// These operations should return adequate representations of these operations.

// You will need to represent functions as well. A definition similar to
// this one might come in handy.
case class Fun(f: Exp => Exp) extends Exp // untyped
case class Fun[S,T](f: Exp[S] => Exp[T]) extends Exp[S=>T] // typed
// To analyze a function in that representation, you can apply f
// to some dummy expression.