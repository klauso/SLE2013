// ==================
// In-Class Exercise:
// ==================

// Write a class Stack (and possibly other classes) which supports
// methods push, pop, top and isEmpty such
// that the following code compiles and runs without errors
class Fruit
class Apple extends Fruit
class Orange extends Fruit

val s1 : Stack[Apple] = EmptyStack.push(new Apple)
val s2 : Stack[Orange] = EmptyStack
val f : Fruit = s1.push(new Orange).top
val s4 : Stack[Object] = s1.pop
val s5 : Stack[Fruit] = s1.push(new Orange)


def size[A](x: Stack[A]) : Int = x match { 
    case EmptyStack => 0
    case Stack(e,r) => 1+size(r) 
}

assert(size(s1)== 1)



	
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

