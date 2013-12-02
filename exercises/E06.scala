object ParsingHomework extends App {
  // Use the parser combinators from the lecture.
  import Parsing.Semantics._

  // Task 1: Implement a parser for numbers
  //
  // 1a) You can start with a parser for digits
  // that returns the digit as a character.
  //
  // digit ::= "0" | "1" | ... | "8" | "9"
  def digit: Parser[Token] = ???
  println(digit("123") + " should be " + Success(1, "23"))
  println(digit("abc") + " should be " + Failure("..."))

  // 1b) Then write a parser for digits
  // that returns the digit as a number.
  def digitAsNumber: Parser[Int] = ???

  // 1c) Now, write a parser for numbers.
  //
  // number ::= digit | digit number
  def number: Parser[Int] = ???
  println(number("315hello") + " should be " + Success(315, "hello"))
  println(number("hello315") + " should be " + Failure("..."))

  // 1d) Add syntactic sugar.
  //
  // A better syntax for alternative(a, b) would be a | b.
  // Provide this syntax for the existing parser language.
  // Which other syntactic shortcuts can you think of?
  // Can you implement all of them in Scala?

  // 1e) Add derived operations.
  //
  // The grammar for numbers can also be written as:
  //
  // number ::= digit+
  //
  // Implement the + and * operations as a derived operations.
  
  def oneOrMore[A](p : Parser[A]) : Parser[Seq[A]] = ???
  def zeroOrMore[A](p : Parser[A]) : Parser[Seq[A]] = ???

  println(oneOrMore(digit)("123abc") + " should be " + Success(Seq(1, 2, 3), "abc"))
  println(oneOrMore(digit)("abc") + " should be " + Failure("..."))
  println(zeroOrMore(digit)("123abc") + " should be " + Success(Seq(1, 2, 3), "abc"))
  println(zeroOrMore(digit)("abc") + " should be " + Success(Seq(), "abc"))
  
  // Task 2: Implement a more complex parser
  //
  // Choose whether to do the basic exercises or the advanced ones (or both).
  //
  // Basic exercises encourages you to explore combinator parsing
  // and the idea of language embedding more.
  //
  // Advanced exercises encourage you to think deeper about
  // the interaction of embedded and host language and
  // the role of parsing in a language toolchain.
  //
  // If you're already familiar with parser combinators,
  // you might find the advanced exercises more interesting,
  // but not necessarily easy.

  // Basic Task 2a) Implement a parser for arithmetic expressions
  //
  // expr ::= addend (("+" | "-") expr)?
  // addend ::= factor (("*" | "/") expr)?
  // factor ::= number

  def expr: Parser[Int] = ???
  def addend: Parser[Int] = ???
  def factor: Parser[Int] = ???

  println(expr("1+2*3") + " should be " + Success(7, ""))

  // Basic Task 2b) Add derived operations
  //
  // Implement a derived operation that captures
  // the similarity between expr and addend.
  
  // Basic Task 2c) Equivalences
  //
  // Try to implement the parser for expressions as follows:
  //
  // expr ::= (expr ("+" | "-"))? addend
  //
  // What happens if you try this variant of the expr parser?
  // Why? Can we change the Parser implementation to avoid this problem?

  // Advanced Task 2a) Implement a parser for grammars.
  //
  // These tasks are very advanced. If you can't manage, consider falling
  // back to the basic tasks, or explain what you tried, why it failed,
  // and what you learned.
  //
  // You can choose your own design, here is just a proposal to get you started.
  sealed trait AST
  case class Terminal(token: Token) extends AST
  case class Nonterminal(name: String, children: Seq[Token]) extends AST

  def grammar: Parser[Parser[AST]] = ???
  
  // Start with token, sequence and alternative, then add the * operator.
  // At this point, you have regular expressions, more or less! Then try
  // to add recursion :)

  // Advanced Task 2b) Can you write the grammar of grammars?
  def hopeForSuccess[A](result: Result[A]): A = result match {
    case Success(result, Seq()) => result
    case Success(_, leftover) => sys.error("leftover tokens: " + leftover)
    case Failure(msg) => sys.error("failure: " + msg)
  }
  val grammarOfGrammars: String = ???
  val parserOfGrammars: Parser[AST] = hopeForSuccess(grammar(grammarOfGrammars))
  val astOfGrammarOfGrammars: AST = hopeForSuccess(parserOfGrammars(grammarOfGrammars)) 

  // Advanced Task 2c) Can you also write an interpreter for grammars?
  def parse(grammar: AST): Parser[AST] = ???

  // Advanced Task 2d) Now, can your grammar of grammars parse itself?
  val penultimateTest : AST = hopeForSuccess(parse(astOfGrammarOfGrammars)(grammarOfGrammars))
  print(penultimateTest + " should be " + astOfGrammarOfGrammars)

  // Advanced Task 2e) And can it parse itself again?
  val ultimateTest : AST = hopeForSuccess(parse(penultimateTest)(grammarOfGrammars))
  print(ultimateTest + " should be " + astOfGrammarOfGrammars)
}