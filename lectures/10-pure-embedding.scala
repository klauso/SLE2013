// 1. Overview (on whiteboard)
// ===========================

// 1a. In programming languages, we have *syntax* and *semantics*.
//
//                 SYNTAX                           SEMANTICS
//
// Syntax is about what you can write.
// Semantics is about what it means.

// 1b. We distinguish abstract syntax and concrete syntax.
//
//                           ABSTRACT
//         TEXT  ---parse-->  SYNTAX                SEMANTICS
//                             TREE
//
// Concrete syntax is the sequence of characters that programmers write.
// Abstract syntax is the structure of a program.
// A parser transforms concrete syntax into abstract syntax.
//
// Last week's lecture was about syntax, this week is about semantics.

// 1c. There are many ways to define semantics.
// Today, we talk about denotational semantics.
//
//                       ABSTRACT                  DENOTATIONAL
//     TEXT  ---parse-->  SYNTAX                    SEMANTICS
//                         TREE
//
// For denotational semantics, we need to answer the questions:
//
//   - What *are* our domain concepts
//     (for example, "what is a region?")
//   - How to *compute* the operations
//     (for example, "how to compute the 'union' of two regions?")

// 1d. There are two ways to relate abstract syntax to denotational semantics.
//
//                       ABSTRACT  ---evaluate-->  DENOTATIONAL
//     TEXT  ---parse-->  SYNTAX                    SEMANTICS
//                         TREE    ---compile--->
//
// An evaluator (or interpreter) looks at the structure of the program
// and directly executes the semantics.
//
// A compiler looks at the structure of the program and generates code
// in some other language that will execute the semantics when run itself.

// 1e. You can start the design of a language at any place in this picture:
//
//   - with the concrete syntax (write example programs)
//   - with the abstract syntax (define the AST)
//   - with the "what is ...?" question of denotational semantics
//   - with the "how to compute ...?" question of denotational semantics
//
// Today, we look at two examples where we start with the "what is ...?"
// question of denotational semantics:
//
//   - a language for describing two-dimensional regions
//   - a language for parsing.

// 2. Regions
// ==========
//
// We now implement the semantics of the Regions language. You saw its syntax
// last week in the lecture on Antlr. This part of the lecture is based on:
//
//   Paul Hudak.
//   Modular Domain-Specific Languages and Tools.
//   In Proc. of International Conference on Software Reuse.
//   IEEE Computer Society, 1998.
//
//   http://haskell.cs.yale.edu/wp-content/uploads/2011/01/DSEL-Reuse.pdf
//
// This is a very good and influential paper that you might want to read to
// deepen your understanding of embedded domain-specific languages.

object Regions {
  // 2a. To define the semantics, we answer the questions.
  object Semantics {
    // What is region?
    //
    // A region is a set of points,
    // represented by its characteristic function.
    type Region = (Double, Double) => Boolean

    // How to compute 'union'?
    //
    // A point is in a union
    // if it is one or the other region.
    def union(r1: Region, r2: Region): Region =
      (x, y) => r1(x, y) || r2(x, y)

    // How to compute 'unitcircle'?
    //
    // A point is in the unitcircle
    // if it is less than 1 away from the origin
    def unitcircle(): Region =
      (x, y) => x * x + y * y <= 1

    // How to compute 'scale'?
    //
    // A point is in a scaled region
    // if the anti-scaled point is in the original region.
    def scale(n: Double, r: Region): Region =
      (x, y) => r(x / n, y / n)

    // How to compute 'translate'?
    //
    // A point is in the translated region
    // if the anti-translated point is in the original region.
    def translate(dx: Double, dy: Double, r: Region): Region =
      (x, y) => r(x - dx, y - dy)
  }

  // 2b. We represent abstract syntax trees with case classes (as last week).
  // See also "composite pattern".

  object Syntax {
    sealed trait Region
    case class Union(r1: Region, r2: Region) extends Region
    case object UnitCircle extends Region
    case class Scale(n: Double, r: Region) extends Region
    case class Translate(dx: Double, dy: Double, r: Region) extends Region
  }

  // 2c. An evaluator (or interpreter) looks at the structure of the program
  // and directly executes the semantics.
  //
  // The operations from 2a correspond to the syntactic constructs from 2b,
  // so the interpreter is very simply and directly maps syntax to semantics.

  def eval(program: Syntax.Region): Semantics.Region =
    program match {
      case Syntax.Union(r1, r2)        => Semantics.union(eval(r1), eval(r2))
      case Syntax.UnitCircle           => Semantics.unitcircle()
      case Syntax.Scale(n, r)          => Semantics.scale(n, eval(r))
      case Syntax.Translate(dx, dy, r) => Semantics.translate(dx, dy, eval(r))
    }

  // 2d. An interpreter is called *compositional* if it defines the meaning
  // of a program in terms of the meaning of its subprograms.
  //
  // Often, a compositional semantics is easier to understand and reason about.
  //
  // If we follow the pattern from 2c to map each syntactic construct directly
  // to a semantic operation, the interpreter is automatically compositional.

  // 2e. A compiler looks at the structure of the program and generates code
  // in some other language that will execute the semantics when run itself.
  //
  // We can transform the interpreter from 2c into a compiler by adding some
  // quotation marks and annotations for string interpolation.

  def compile(program: Syntax.Region): String =
    program match {
      case Syntax.Union(r1, r2)        => s"Semantics.union(${compile(r1)}, ${compile(r2)})"
      case Syntax.UnitCircle           => s"Semantics.unitcircle()"
      case Syntax.Scale(n, r)          => s"Semantics.scale($n, ${compile(r)})"
      case Syntax.Translate(dx, dy, r) => s"Semantics.translate($dx, $dy, ${compile(r)})"
    }

  // Note that
  //
  //   s"Semantics.union(${compile(r1)}, ${compile(r2)})"
  //
  // means the same thing as:
  //
  //   "Semantics.union(" + compile(r1) + ", " + compile(r2) + ")"
  //
  // See http://docs.scala-lang.org/overviews/core/string-interpolation.html.

  // 2f. The interpreter or compiler are rather boring, just mapping syntax to
  // semantics. All of the interesting work happens in the semantics. So we
  // can ignore the parser and interpreter or compiler entirely, and call the
  // methods in the semantics directly.
  //
  // This approach is called "pure embedding".

  object EmbeddedProgram {
    import Semantics._

    val myRegion = union(translate(1, 5, unitcircle()), unitcircle())
  }

  // This is a Scala program. But it looks exactly like a Regions program. We
  // say we embedded Regions into Scala, and that Scala is the host language
  // for this embedding.

  // 2g. Pure Embedding has the benefit that we can reuse the concrete and
  // abstract syntax, the type system, and all kinds of other tool support
  // of the host langauge.
  //
  // For example, if you uncomment the following Regions program, your Scala
  // IDE should show you an error:

  /*
  object WrongProgram {
    import Semantics._

    val myRegion = unin(translate(1, 5, unitCircle()), unitCircle())
  }
  */

  // So you can use your Scala IDE also as a Regions IDE, just because we
  // embedded Regions into Scala. As an implementor of the Regions DSL, I
  // don't have to perform any extra work to provide this IDE to the Regions
  // programmers.
}

// 3. Parsing
// ==========
//
// As a second example, we now embed a language for parsing into Scala.
// This part of the lecture is based on:
//
//   Adriaan Moors, Frank Piessens, and Martin Odersky.
//   Parser Combinators in Scala.
//   Technical Report CW 491.
//   Dept. of Computer Science, Univ. Leuven, 2008.
//
//   https://lirias.kuleuven.be/handle/123456789/164870
//
// This technical report explains both the implementation and the use of
// the parser combinators provided by the Scala standard library.

object Parsing extends App {
  // 3a. To define the semantics, we answer the questions.
  object Semantics {
    // What is a parser?
    //
    // Initial answer:
    // A parser is a function from inputs to results.
    //
    //   type Input = ???
    //   type Result = ???
    //   type Parser = Input => Result
    //
    // First refinement:
    // An input is a sequence of tokens.
    //
    //   type Input = Seq[Token]
    //   type Result = ?
    //   type Parser = Input => Result
    //
    // Second refinement:
    // A result is either success or failure.
    //
    //   type Input = Seq[Token]
    //   trait Result
    //   case class Success() extends Result
    //   case class Failure() extends Result
    //   type Parser = Input => Result
    //
    // Third refinement:
    // In case of success, we return a value.
    // In case of failure, we return a message.
    //
    //   type Input = Seq[Token]
    //   trait Result[+A]
    //   case class Success[+A](value: A) extends Result[A]
    //   case class Failure(message: String) extends Result[Nothing]
    //
    // Final refinement:
    // In case of success, we also return the remaining input.

    type Token = Char
    type Input = Seq[Token]

    sealed trait Result[+A]
    case class Success[+A](value: A, next: Input) extends Result[A]
    case class Failure(message: String) extends Result[Nothing]

    type Parser[+A] = Input => Result[A]

    // We often have to refine the answer to the "what is ..." question this
    // way. Sometimes, we have to come back to this question while we are
    // implementing the operations. 

    // How to accept the next token?
    //
    // Succeed if there's a token left.
    def token: Parser[Token] =
      (input) => input match {
        case result +: rest => Success(result, rest)
        case Seq()          => Failure("no token found")
      }

    // How to accept only specific values?
    //
    // Parse and then check the value.
    def filter[A](f: A => Boolean, p: => Parser[A]): Parser[A] =
      (input) => p(input) match {
        case Success(value, next) if f(value) => Success(value, next)
        case Success(_, _)                    => Failure("we don't like this value")
        case Failure(message)                 => Failure(message)
      }

    // How to compose two parsers in sequence?
    //
    // Parse with one parser, if it succeeds, parse the remaining input with
    // the other parser.
    def sequence[A, B](p: Parser[A], q: => Parser[B]): Parser[(A, B)] =
      (input) => p(input) match {
        case Success(result1, next1) => q(next1) match {
          case Success(result2, next2) => Success((result1, result2), next2)
          case Failure(message2)       => Failure(message2)
        }
        case Failure(message1) => Failure(message1)
      }

    // How to compose two parsers as alternatives?
    //
    // Parse with one parser, if it fails, parse the same input with the
    // other parser
    def alternative[A](p: Parser[A], q: => Parser[A]): Parser[A] =
      (input) => p(input) match {
        case Success(result1, next) => Success(result1, next)
        case Failure(message1)      => q(input)
      }

    // How to perform semantic actions?
    //
    // Parse and transform the value returned.
    def map[A, B](p: => Parser[A], f: A => B): Parser[B] =
      (input) => p(input) match {
        case Success(result1, next1) => Success(f(result1), next1)
        case Failure(message1)       => Failure(message1)
      }
  }

  // 3b. Here are some example parsers.
  import Semantics._

  print(token("hello"))
  println(" should be Success(h,ello)")

  print(sequence(token, token)("hello"))
  println(" should be Success((h,e),llo)")

  // 3c. In a programming language, there are two kinds of operations:
  //
  // - Primitive operations are implemented in terms of the semantics.
  // - Derived operations are implemented in terms of the primitive operations.
  //
  // Here is a derived operation for parsing a specific token:
  def accept(expected: Token): Parser[Token] =
    filter[Token](expected == _, token)

  // An important benefit of pure embedding is that users of the language can
  // add their own derived operations. This is usually not possible if we
  // implement a language by writing our own compiler, because in that case,
  // we have to change the compiler if we want to add more operations.

  // 3d. Here are some example parsers we can express using the derived
  // operation from 3c.

  print(accept('h')("hello"))
  println(" should be Success(h,ello)")

  print(accept('h')("bye"))
  println(" should be Failure(we don't like this value)")

  def eitherBorH = alternative(accept('b'), accept('h'))

  print(eitherBorH("hello"))
  println(" should be Success(h,ello)")

  print(eitherBorH("bye"))
  println(" should be Success(b,ye)")

  print(eitherBorH("wrong"))
  println(" should be Failure(we don't like this value)")
}

// 4. Summary
//
// Today's lecture was about semantics. To be more specific, it was about
// denotational semantics. In denotational semantics, you need to say what
// each program means in a compositional way. I showed you one approach to
// defining the denotational semantics where you
//
//  - start by asking "what is ..." questions
//    and write type definitions for each domain concept,
//
//  - then ask "how to compute" questions
//    and write method definitions for each primitive operation.
//
// It would then be easy to write an compositional interpreter or compiler,
// but you can also just use pure embedding. That is, you let the programmers
// of your language work in the host language you wrote the type and method
// definitions in, and you let them call these method definitions directly.
//
// This allows the programmers to reuse existing tools for the host language,
// and it allows them to implement their own derived operations.
