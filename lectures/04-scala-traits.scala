/* Software Language Engineering Course 
 * University of Marburg
 * Programming Languages and Software Engineering Group
 */
 
 /* These notes are in part based on the book "Programming in Scala"
  * by Martin Odersky et al */
 

// =====================
// Scala Class Hierarchy
// =====================

// See http://www.scala-lang.org/old/sites/default/files/images/classhierarchy.png

// ============
// Case Classes 
// ============

/* 
Case classes are regular classes which export their constructor parameters and 
which provide a recursive decomposition mechanism via pattern matching.
Case classes also provide implementations for structural equality and
hash code computation, copy constructors, and factory functions in the
companion object.

Here is a sample case class:
*/

case class Student(name: String, matrNr : Int) 

// Case classes can be instantiated without "new"

val s = Student("Heinz", 12345)

// Their components are public

val name = s.name

// Equality is structural

assert(s == Student("Heinz", 12345))

// Equal objects have the same hash code

assert(s.hashCode == Student("Heinz",12345).hashCode)

// Copy constructors that allow one to change parts of the object are available.

assert(s.copy(name="Joe") == Student("Joe",12345))

// Objects of case classes can be decomposed via pattern matching.

assert((s match { case Student(n,m) => m }) == 12345)


/* The semantics of case classes can loosely be described via the following two definitions, 
which are roughly equivalent to the semantics of the case class defined above: */

class Student(val name: String, val matNr: Int) {
  def canEqual(x: Any): Boolean = x.isInstanceOf[Student];

  override def equals(other: Any) : Boolean = 
    if (other.isInstanceOf[Student]) {
	  val otherStudent = other.asInstanceOf[Student]
	  (otherStudent.canEqual(this)) && otherStudent.name == name && otherStudent.matNr == matNr
	} else false
  override def toString = "Student(" + name.toString + "," + matNr.toString + ")"	
  override def hashCode = name.hashCode + matNr.hashCode + 777
  def copy(name : String = name, matNr: Int = matNr) = new Student(name,matNr)
}	
object Student {
  def apply(name: String, matNr: Int) = new Student(name, matNr)
  def unapply(s: Student) : Option[(String, Int)] = if (s != null) Some(s.name, s.matNr) else None
}  

// ======
// Traits
// ======

/*
Traits are sets of (possibly abstract) method and field definitions that can be used to assemble classes.
Traits look similar to classes, but they cannot have constructors. An arbitrary number of traits
can be mixed into a class. In Scala, they have the role of reusable building blocks for classes.
They can also be used as types and hence as interfaces (Scala has no separate interface construct).

Note that the word "trait" is used to denote something related but different in the Smalltalk world, hence
if you see a reference to the word "trait" somewhere, check the community for which the text is written.

Here is a good example of how traits can be used (and are in fact used in the Scala standard library): */

trait Ordered[A] extends Any  {

  /** Result of comparing `this` with operand `that`.
   *
   * Implement this method to determine how instances of A will be sorted.
   *
   * Returns `x` where:
   *
   *   - `x < 0` when `this < that`
   *
   *   - `x == 0` when `this == that`
   *
   *   - `x > 0` when  `this > that`
   *
   */
  def compare(that: A): Int

  /** Returns true if `this` is less than `that`
    */
  def <  (that: A): Boolean = (this compare that) <  0

  /** Returns true if `this` is greater than `that`.
    */
  def >  (that: A): Boolean = (this compare that) >  0

  /** Returns true if `this` is less than or equal to `that`.
    */
  def <= (that: A): Boolean = (this compare that) <= 0

  /** Returns true if `this` is greater than or equal to `that`.
    */
  def >= (that: A): Boolean = (this compare that) >= 0

  /** Result of comparing `this` with operand `that`.
    */
  def compareTo(that: A): Int = compare(that)
}

case class Rational(n: Int, d: Int) extends Ordered[Rational] {
  def compare(that: Rational) = this.n*that.d-that.n*this.d
}  

assert(Rational(2,3) < Rational(5,6))

// By implementing "compare", the Rational type gets a number of other
// convenient derived methods "for free". Compare this with the situation
// that you would have to implement all these derived operations by hand
// in all data types that can be compared.

// In single inheritance languages, one would need to consume the inheritance
// link to inherit these methods. In Scala, a class can mix in an arbitrary number
// of traits in addition to inheriting from another class. For instance, let's
// assume we want to create a subclass of scala.swing.TextField that is ordered
// according to its text. We can then use the "with" clause to mix in an additional trait.

import scala.swing.TextField

class MyTextField extends TextField with Ordered[MyTextField] {
  def compare(that: MyTextField) = this.text.compare(that.text)
}  

// It is also possible to _stack_ traits. Unlike in multiple inheritance languages, traits
// are subject to a process called _linearization_. This means that the traits are arranged
// according to a topological sort of the inheritance graph. This can be used to implement
// stackable modifiers to a class. Here is an example:

abstract class Person(name : String) {
  def salutation : String = name
}

trait DoctoralDegree extends Person {
  override def salutation = "Dr. "+super.salutation 
}  

trait Lord extends Person {
  override def salutation = "Lord "+super.salutation
}

// The "with" clause can not only appear in supertype declarations but also in type declarations 
// and constructor invocations. We use that here to create variants on demand.
val p1 = new Person("Smith") with DoctoralDegree
val p2 = new Person("Jones") with Lord
val p3 = new Person("Meyer") with Lord with DoctoralDegree
val p4 = new Person("Fischer") with DoctoralDegree with Lord

assert(p1.salutation == "Dr. Smith")
assert(p2.salutation == "Lord Jones")
assert(p3.salutation == "Dr. Lord Meyer")
assert(p4.salutation == "Lord Dr. Fischer")

// The last two examples illustrate that the order of traits matters. The exact rules are a little
// complicated, but roughly, the right-most declaration "wins" if there is a conflict.

// It is also possible to keep a similar design as the one above even if "saluation" is abstract
// in class Person. In that case, the modifier "abstract"  must be added to those definitions that
// call "super".  The "abstract override" annotation means that the trait can only be mixed into a class
// that contains a concrete definition of salutation.