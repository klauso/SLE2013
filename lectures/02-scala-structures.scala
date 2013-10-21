
/* Software Language Engineering Course 
 * University of Marburg
 * Programming Languages and Software Engineering Group
 */
 
 /* These notes are in part based on the book "Programming in Scala"
  * by Martin Odersky et al */
 
// =======
// Classes
// =======
 
 // This is what a class looks like in Scala
 
 class ChecksumAccumulator {
   private var sum = 0
   def add(b: Byte) { sum += b }
   def checksum() : Int = ~(sum & 0xff) + 1
 }
 
 // If a method definition does not have an equal sign before 
 // the body, the result type will be Unit
 
 // Example: The return type of f is Unit, but the return type of g is string.
 def f() { "hi" }
 def g() = { "hi" }
 
 
 // Objects of a class can be instantiated with new
 
 val acc = new ChecksumAccumulator
 
 // Semicolons in Scala are not necessary most of the time.
 // They can be used to have multiple statements on one line.
 
 var s = "hello"; println(s)
 
 // Classes in Scala do not have static members.
 // Instead, Scala has a more powerful (and more object-oriented)
 // way to achieve the same effect: Singleton objects
 
 // Singleton classes look like a class but use the keyword "object"
 // instead of "class". Their name becomes a globally (where it 
 // is imported) visible  identifier
 
 object FunnyChecksumAccumulator extends ChecksumAccumulator {
   override def add(b: Byte) { println("hihi"); super.add(b) }
}   

FunnyChecksumAccumulator.add(5)

// Singleton objects are more flexible than static methods/fields
// in Java, because singleton objects are objects and can hence
// be passed to methods, be used polymorphically, etc.

// Singleton objects that have the same name as a class (and are
// declared in the same file) are called _companion objects_.
// A companion object can access private members of its companion 
// class and vice versa. They are used extensively in the Scala
// standard library

// Singleton objects can also be used as entry points into an application.
// This is particularly relevant when an application is compiled
// with scalac and not executed via the Scala REPL.
// Any singleton object that contains a "main" method which takes
// an array of strings as argument can be enty point.

// If the command line arguments are not used, then a main object
// can be written in a more concise way by inheriting from
// the trait scala.Application

// ===========
// Basic Types
// ===========

// Scala provides a set of base types similar to the one in most 
// other statically typed languages: Byte, Int, Long, Char, Boolean etc.

// A slightly more unusual but quite useful base type is Symbol. Here
// is a symbol literal:

val someSymbol = 'hi

// There is not much that one can do with symbols except to compare them.
// The main feature of symbols (compared to Strings) is that they
// are automatically "interned", which means that equal symbols are identical.

assert(someSymbol == 'hi)

// Basic types are also treated in an object-oriented way in Scala.
// In particular, they have methods.
// The standard operator syntax is just syntactic sugar to call 
// some of these methods.

val x = 1 + 2

// is just syntactic sugar for

val x = (1).+(2)

// In other languages, such features are hard-wired into the language.
// Scala, on the other hand, is flexible enough to extend (via libraries) 
// it with new types that have the same look and feel as the built-in types
// This embodies the idea of "Growing a language" as proposed by Guy Steele.

// Any method can be an operator. For instance, instead of
FunnyChecksumAccumulator.add(5)
// we can just as well write
FunnyChecksumAccumulator add 5

// A small set of identifiers is reserved for prefix operators: +,-,!,~
// These methods are written as unary_+, unary_- and so forth.
// Hence
-2
// is just a shorthand for 
(2).unary_-
// Programmers can define these prefix operators for their own types, too.

// It is also possible to define postfix operators, but their use in Scala
// is discouraged because it interacts poorly with semicolon interference

// The same applies to boolean values, of course.
// For instance
true || false
// is just shorthand for
true.||(false)

// However, boolean operations are, as usual, short-circuited.
// When this expression is evaluated, only "hi" but not "ho" is printed.
{ println("hi"); true} || {println("ho"); false }
// You may wonder how this interacts with Scala's argument evaluation
// strategy, which is to evaluate arguments before executing the call
// (also known as "call by value")
// Answer: There is a facilty to delay arguments of method calls which
// you have not seen yet.

// Object equality is determined by the == method. Of course,
"asdf" == "fdsa"
// is once more just a shorthand for
"asdf".==("fdsa")

// "==" is a method that is also understood by the null value. Hence
// all of these comparisons are ok and yield false (and not a runtime error)
"asdf" == null
null == "asdf"
null.==("asdf")

// Precedence and associativity for operators is hard-wired according
// to a table in the language specification

// =====================
// Object initialization
// =====================


// A class has a primary constructor which can have named and typed parameters.
// These parameters are private and immutable.
// Top-level statements and initialization expressions are also executed when
// the constructor runs.

class Rational(n: Int, d: Int) {
    println("Created "+ n +"/"+ d)
}

new Rational(5,3)

// Additional constructors can be defined, but they must call the primary constructor
// (directly or indirectly via other additional constructors)

// Overriding an inherited method requires the annotation "override".
// This means that the compiler can detect more errors statically (such as a typo in the method name).

// Example:

class Rational(n: Int, d: Int) {
    println("Created "+ n +"/"+ d)
	def this(n: Int) = this(n, 1) // auxiliary constructor
	override def toString = n + "/" +d
}

new Rational(27)


// Here is a more complete example that also defines a set of methods to perform
// arithmetic with rational numbers. It overloads the methods with different 
// types such that we can also add or multiply by an integer.

 class Rational(n: Int, d: Int) {

    require(d != 0)

    private val g = gcd(n.abs, d.abs)
    val numer = n / g
    val denom = d / g

    def this(n: Int) = this(n, 1)

    def + (that: Rational): Rational =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

//    def + (i: Int): Rational =
//      new Rational(numer + i * denom, denom)

    def - (that: Rational): Rational =
      new Rational(
        numer * that.denom - that.numer * denom,
        denom * that.denom
      )

    def - (i: Int): Rational =
      new Rational(numer - i * denom, denom)

    def * (that: Rational): Rational =
      new Rational(numer * that.numer, denom * that.denom)

    def * (i: Int): Rational =
      new Rational(numer * i, denom)

    def / (that: Rational): Rational =
      new Rational(numer * that.denom, denom * that.numer)

    def / (i: Int): Rational =
      new Rational(numer, denom * i)

    override def toString = numer +"/"+ denom

    private def gcd(a: Int, b: Int): Int = 
      if (b == 0) a else gcd(b, a % b)
}

// The following tests work fine:

val x = new Rational(4,6)
x*x
x*2

// However, this one does not:

// 2*x  -- error: type int does not have a definition of * for Rational

// We can fix this problem by defining an _implicit conversion_.
// Implicit conversions are enabled by the following import
import scala.language.implicitConversions

// Implicit conversions are implicit function values or implicit methods
// that take one argument type and produce a result of a different type.
// A call to an implicit conversion is automatically injected by the Scala
// compiler if it can restore type safety.

// This means that we can leave out all the overloaded methods and instead
// add a single implicit conversion, like so:

class Rational(n: Int, d: Int) {

    require(d != 0)

    private val g = gcd(n.abs, d.abs)
    val numer = n / g
    val denom = d / g

    def this(n: Int) = this(n, 1)

    def + (that: Rational): Rational =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

    def - (that: Rational): Rational =
      new Rational(
        numer * that.denom - that.numer * denom,
        denom * that.denom
      )

    def * (that: Rational): Rational =
      new Rational(numer * that.numer, denom * that.denom)

    def / (that: Rational): Rational =
      new Rational(numer * that.denom, denom * that.numer)

    override def toString = numer +"/"+ denom

    private def gcd(a: Int, b: Int): Int = 
      if (b == 0) a else gcd(b, a % b)
}

implicit def intToRational(x: Int) = new Rational(x)

// Now all of these work
val x = new Rational(4,6)
x*x // expanded to x.*(x)
x*2 // expanded to x.*(intToRational(2))
2*x // expanded to intToRational(2).*(x)


// Implicit conversions are a quite powerful feature, but it can also make code
// hard to read and understand. Hence the latest Scala version requires an
// explicit activation via import scala.language.implicitConversions


// =========
// Functions
// =========

// There are two ways to define functions in Scala:
// 1) As members of an object. Such functions are typically called _methods_.
// 2) As first-class functions which evaluate to a function value (a.k.a. closure)

// You have already seen how to define Java-like methods inside classes.
// Let's look at some other features.

// Functions can be _local_. That is, we can define a new function within
// an arbitrary scope. Local functions can access variables from their enclosing
// environment and are not visible outside their scope.

// Example:

def f(x: Int) = {
  def g(y: Int) = x + y
  g(2)+g(3)
}  
assert(f(5) == 15)

// Scala also allows to define first-class functions, such as this increment function
val inc = (x: Int) => x+1

// Functions being first-class means that they can be passed to and returned from other functions,
// stored in variables, and so forth. For instance

assert( List(1,2,3).map(inc) == List(2,3,4))

// The argument type can be inferred if it is clear from the context.
// For instance, when calling "map" on a List of Ints, it must be Int
// and must hence not be given:

List(1,2,3).map( (x) => x+1)

// If an argument type is inferred, the brackets around the argument name
// can also be ommitted.

List(1,2,3).map( x => x+1)

// A particularly concise way to define a syntax is via _placeholder syntax_.
// Here is the same example expressed with placeholder syntax.

List(1,2,3).map(_+1)

// If multiple placeholders show up, the expression is interpreted as a multi-argument
// function, where the leftmost placeholder stands for the first argument, the second
// one for the second argument, and so forth.

// Example:
assert(List(1,2,3).reduce(_+_) == 6)

// The underscore also has a different function. It can be used to turn methods into
// first-class functions.

val r = List(2,3,4).reduce _
assert(r(_+_) == 9)
assert(r(_*_) == 24)

// Objects can also be used with function call syntax. If an object has a method
// with the special name "apply", the apply function can be called using function
// call syntax.

object Test {
  def apply(x: Int) = x+1
}

assert( Test(5) == 6)  

// Of course we can also use the explicit call syntax.

assert(Test.apply(5) == 6)

// The apply method is used extensively in the standard Scala library.
// For instance, we can retrieve the second element of a list like so
val myList = List(1,2,3,4,5)
assert(myList(2) == 3)
val myMap = Map( "x" -> 1, "y" -> 2)
assert(myMap("x") == 1)

// Another common use case is to use the apply function of companion objects
// as factory methods for the corresponding class.
// For instance, the initializer for myList above calls the "apply" method
// of the companion object of the List class

// First-class functions are evaluated to closures, which means that 
// it evaluates to a function in the environment in which it was created.
// Example:

def makeAdder(x: Int) = (y:Int) => y+x
val addseven = makeAdder(7) // The binding x=7 is stored together with the function in the closure
assert(addseven(2) == 9)

