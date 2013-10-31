// Consider the following class hierarchy

abstract class Food {
  val calories : Int
}  
case class Grass(c: Int) extends Food { val calories = c}
case class Fish(c: Int) extends Food { val calories = c}

// Define three classes: Animal, Cow, Shark. Cow and Shark
// should be subtypes of Animal. All animals should have
// a method "eat" and a method "uneat", which returns the 
// previously eaten food (if available).

// Cows eat grass only and sharks eat fish only. Make 
// sure that the type checker catches all violations 
// of this invariant.

val shark = new Shark
val cow = new Cow
val grass = new Grass(10)
val fish = new Fish(20)

// this should work
cow.eat(grass)
shark.eat(fish)

// these should be rejected as type errors
// cow.eat(fish)
// shark.eat(grass)

// it should be possible to have a collection of different animals
val l = List(shark,cow)

// Make a method like this work:
def eatBetterFood(animal: ?, food1: ?, food2: ?) : Unit = 
  if (food1.calories > food2.calories) animal.eat(food1) else animal.eat(food2)

// Make a method like this work:  
def transferFood(animala: ?, animalb: ?) {
  animala.uneat match {
    case Some(f) => animalb.eat(f)
	case None => {}
  }
}  


// ===================
// Homework Assignment
// ===================

/*
Homework is due on Nov 6, 23:59 via email
to kos@informatik.uni-marburg.de
Please choose the subject title "SLE03"

Read this:
http://www.scala-lang.org/old/node/111
and then Section 6.19 in the Scala Specification
http://www.scala-lang.org/docu/files/ScalaReference.pdf
(a sequence comprehension is the same thing as a for comprehension)

Now consider the following data structures: */

case class Author(name: String)
case class Book(title: String, authors: List[Author])

// Here is a sample list of books
val books : List[Book] = 
  List(Book("Compiler Construction", List(Author("Aho"), Author("Sethi"), Author("Ullman")))
       Book("Scala Spec", List(Author("Odersky"))),
       Book("The Compiler Design Handbook", List(Author("Shankar"))))	   

/* Now design (using sequence comprehensions) a program that
takes a list of books, filters all books whose title contains the word "Compiler",
and assembles a list of all author names in uppercase letters of all those books.
For instance, in the example from above the result of that program should be :*/
	   
 List("AHO", "SETHI", "ULLMAN", "SHANKAR")	   
 
/* Your second task is to desugar the sequence comprehension by hand using the
rules given in Sec. 6.19 of the Scala Specification. Confirm by tests that your
desugared version of the query agrees with the sequence comprehension. */