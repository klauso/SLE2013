/* Software Language Engineering Course 
 * University of Marburg
 * Programming Languages and Software Engineering Group
 */
 
 /* These notes are in part based on the book "Programming in Scala"
  * by Martin Odersky et al */
 

// Probably you know the concept of abstract methods from other OO languages.
// Scala is more general and also allows you to define other abstract members
// of a type: Abstract vals, abstract vars, abstract defs, and abstract types.
// Here is an example of a type that has all of them:

trait Abstract {
  type T // an abstract type
  def foo(x: T) : T // an abstract def
  val x : T // an abstract val
  var y : T  // an abstract var

// Here is a concrete type that fills in definitions for all abstract members

class Concrete extends Abstract {
  type T = String
  def foo(x: String) = x + x
  val x = "hi"
  val y = x
}
  
// =============
// Abstract Vals
// =============

// An abstract val declaration has the form "val x : T" for some name x and type T.
// The value has to be provided by a concrete val definition in a subclass, which
// has to contain a corresponding "val x = e" declaration for some expression e of type T.

// The Scala compiler makes sure that no class that contains abstract vals without corresponding
// concrete binding will never be instantiated.

// A common use case for abstract vals is to serve as parameters for traits (which cannot
// define constructors). The abstract vals have to be bound in the class using the trait, then.

// Abstract vars are like abstract vals, except of course that they are mutable.

// The initialization order of abstract vals is a complicated matter and potential source
// of confusion. In particular, an abstract val may be used in the constructor code
// before it is initialized. Scala has two mechanisms to deal with such problems:
// Pre-initialized fields and lazy vals. We will not talk about any of these in detail at 
// this point.

