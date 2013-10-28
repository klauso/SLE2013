/* Software Language Engineering Course 
 * University of Marburg
 * Programming Languages and Software Engineering Group
 */
 
 /* These notes are in part based on the book "Programming in Scala"
  * by Martin Odersky et al */
 

// This file can only be compiled with scalac. It won't work as script.

// ========  
// Packages
// ========

// One can use standard Java-like package declarations in Scala.
// The more general way to use packages in Scala is to use nested packages.
// Here is an example:

package a {
  package b {
    class Foo
	class Fooz
  }
  package c {
	class Bar
	object X {
	  val y = 77
	}  
  }
  package d {
    class Foo 
  }
}  

object Main {
  val x = new a.b.Foo
}  

// Packages and objects can be imported via the imports clause.
// We can access a particular type

import a.b.Foo

// after the import we can just say "Foo" when we need it.

// If multiple types within a package should be imported, we
// can write them in the same declaration.

import a.b.{Foo,Fooz}

// We can also import all types in a package:

import a.c._

// Now Bar and X are in scope

// It is also possible to import all members of an object.

import X._

// Now the binding of y in a.c.X is in scope

// If there are conflicts, import statements can perform local renaming.
import a.d.{Foo=>Fu}

// Now the a.d.Foo type can be accessed as Fu; no confusion with a.b.Foo
// Renaming works not only with types but also with sub packages. This is
// often used to give a convenient short alias to a long package name.

// Finally, imports in Scala can be local. That is, they can appear within
// a definition (say, within a class or within a method). This is useful to
// avoid pollution of your name space.


// Three imports are implicit in every file, namely these ones:

import java.lang._
import scala._
import Predef._

// Many Scala features that look hard-wired are actually implemented in one
// these packages. 


