// Tested with Scala 2.10.3

package lecture15
import scala.language.{ higherKinds, implicitConversions, postfixOps }

/*
Goal: implement map fusion.
Motivating example: Consider this code:
val l0 = List(1, 2, 3, ..., 1000 * 1000) map (l0 => l0 + 1)
l0 map (x => x * 2)

We want to transform this code using the transformation rule:
l0 map f map g => l0 map (f andThen g)

where, as a reminder, andThen is defined as follows.
(f andThen g)(l0) = g(f(l0))

The solution will work even if the snippet "l0 map f map g" is not written
directly in one place. 

val l0 = asRep(List(1, 2, 3, ...))
val l1 = if (cond) l0 map f else ...
fuse(l1 map g)
*/

/**
 * In this lecture, we use polymorphic embedding to embed (parts of) Scala
 * within itself, de Brujin indexes to handle binding. The material is
 * closely based on the material of the previous lecture.
 */
trait ClientProgramFragment {
  //A minimal polymorphically embedded program, using addition.
  def prog1(semantics: Base.ClientInterface) = {
    import semantics._
    asRep(1) + 2
  }
}

/**
  * First, we illustrate example client programs in the semantics, and how to apply them.
  */
object Main extends scala.App with Transformations with Utilities with ClientProgramFragment {
  //Run this program using different semantics, and print the results.
  println(prog1(Base.Eval))
  println(prog1(Base.Reification))

  // We can also use a semantics for a bigger language.
  println(prog1(DeBrujinFunctions.Reification))

  // Now, let us write more complicated programs, which use the language
  // interface for Lists operations.
  
  // Note that we optimize listExp2 and we successfully optimize patterns
  // written across it and listExp.
  val baseList = List(1, 2, 3)

  def listExp1(semantics: Lists.ClientInterface) = {
    import semantics._
    (asRep(baseList)) map (x => x + 1)
  }
  def listExp2(semantics: Lists.ClientInterface) = {
    import semantics._
    listExp1(semantics) map (x => x * 2)
  }
  def listExp3(semantics: Lists.ClientInterface) = {
    import semantics._
    listExp2(semantics) map (_ + 2)
  }

  //Now we use ToShortcutFusion, which is a reification semantics translating list primitives to primitives for shortcut fusion.
  
  val beforeFusion = reifyDB(listExp2(Lists.ToShortcutFusion))
  traceDB(beforeFusion)
  //Then, we apply 
  val afterFusion = beforeFusion fold FusionSemantics
  traceDB(afterFusion)

  val betaReduced = afterFusion fold BetaReduceDB
  traceDB(betaReduced)

/*
 * The result of optimization is the following term: 
Build(nil => cons =>
  FoldRight(Const(List(1, 2, 3)),
      nil,
      hd => tl =>
        cons((hd + 1) * 2, tl)))

 * This term is the result of the program expectedResult:
 */
  def expectedResult(semantics: Lists.ClientInterface) = {
    import semantics._
    asRep(baseList) map (x => (x + 1) * 2)
  }

  //So we can reify it according to the same semantics and compare them:
  val expectedResultDBSF = reifyDB(expectedResult(Lists.ToShortcutFusion))
  traceDB(expectedResultDBSF)
  //assert(betaReduced == expectedResultDBSF) //In fact, this fails.
  
  //Why? expectedResultDBSF contains some extra beta-redexes. Let's remove them:
  val actualExpectedResultDBSF = expectedResultDBSF fold BetaReduceDB
  traceDB(actualExpectedResultDBSF)
  assert(betaReduced == actualExpectedResultDBSF)
  //This worked!
  //Thanks to de Brujin indexes, we did not need to write a special
  //implementation of equality, we just reuse structural equality, and this is
  //a big win. Implementing equality for HOAS or nominal terms is much harder. 
  
  //Above, we have produced many intermediate results just to consume them again.
  //We can also perform Fusion and BetaReduction directly.
  val twoStepOptimized = beforeFusion fold FusionBetaReduceDB
  traceDB(twoStepOptimized)

  //Now, let's see what we can do
  //Let's start again from listExp2. Instead of reifying it and then performing shortcut fusion, let's do it at once. 
  val directlyFused = reifyDB(listExp2(DirectFusion))
  traceDB(directlyFused)

  //Let's try to also do beta-reduction. This version uses an implementation of
  //beta-reduction for deBrujin indexes, which is outside the scope of the
  //lecture (even though writing it is *relatively* easy).
  val directlyFusedBetaReducedDB = reifyDB(listExp2(DirectFusionAndBetaReduceDB))
  traceDB(directlyFusedBetaReducedDB)

  //Now, let's try to use instead beta-reduction for HOAS terms.
  val directlyFusedBetaReducedHOAS = reifyHOAS(reifyDB(listExp2(DirectFusion)) fold BetaReduceHOAS)
  trace(directlyFusedBetaReducedHOAS)
}

trait Utilities extends Transformations {
  import Base.Syntax.Exp

  //Reifying de Brujin terms produces a functions from an initial outer depth to the term itself. This function provides the initial outer depth while being more readable than term(0)
  def reifyDB[L <: Base.Semantics, T](term: Int => Exp[L, T]) = term(0)
  //Similarly, here we must provide a list of terms, which however can be empty for a closed term.
  def reifyHOAS[L <: Base.Semantics, T, U](term: List[U] => Exp[L, T]) = term(Nil)

  def toHoas[T](e: Exp[LanguageDBSF, T]) =
    reifyHOAS(e fold ToHoas)

  //Utilities for printing the tree to video.
  def trace[L <: Base.Semantics, T](e: Exp[L, T]): Unit = {
    println()
    println(e)
  }

  def traceDB[T](e: Exp[LanguageDBSF, T]) = {
    trace(e)
    println(toHoas(e))
  }
}

/*
 * This trait contains different objects which assemble together multiple semantic blocks in multiple layers.
 */
trait Transformations {
  import Base.Syntax.Exp

  //DBSF = De Brujin + ShortcutFusion
  type LanguageDBSF = DeBrujinFunctions.Semantics with ShortcutFusion.Semantics

  object FusionSemantics extends DeBrujinFunctions.Reification with ShortcutFusion.FusingReification with DeBrujinFunctions.AsFunctionApp {
    type Language = LanguageDBSF
  }

  object HOASFullSemantics extends HOASAdvanced.Reification with ShortcutFusion.Reification {
    type Language = HOAS.Semantics with ShortcutFusion.Semantics
  }

  object ToHoas extends HOAS.FromDeBrujin with ShortcutFusion.IgnoreContext {
    val base = new HOASAdvanced.Reification with ShortcutFusion.Reification {
      type Language = HOAS.Semantics with ShortcutFusion.Semantics
    }
  }

  object DirectFusion extends Lists.ToShortcutFusion with Lists.ClientInterface with HOAS.Forward {
    val base = new HOAS.ToDeBrujin with ShortcutFusion.IgnoreContext {
      val base = new DeBrujinFunctions.Reification with ShortcutFusion.FusingReification with DeBrujinFunctions.AsFunctionApp {
        type Language = LanguageDBSF
      }
    }
  }

  object BetaReduceDB extends DeBrujinFunctionsAdvanced.BetaReducingReification with ShortcutFusion.Reification {
    outer =>
    type Language = LanguageDBSF
    def substitutor[S] = new DeBrujinFunctionsAdvanced.Substitution[S] with ShortcutFusion.IgnoreContext {
      val base: outer.type = outer
    }
  }
  
  // Do the normalization in one step, as discussed for Jona's question and as done by LMS. This gives the same results.
  object FusionBetaReduceDB extends DeBrujinFunctionsAdvanced.BetaReducingReification with ShortcutFusion.FusingReification with DeBrujinFunctions.AsFunctionApp {
    outer =>
    type Language = LanguageDBSF
    def substitutor[S] = new DeBrujinFunctionsAdvanced.Substitution[S] with ShortcutFusion.IgnoreContext {
      val base: outer.type = outer
    }
  }
  
  object DirectFusionAndBetaReduceDB extends Lists.ToShortcutFusion with Lists.ClientInterface with HOAS.Forward {
    val base = new HOAS.ToDeBrujin with ShortcutFusion.IgnoreContext {
      val base = new DeBrujinFunctionsAdvanced.BetaReducingReification with ShortcutFusion.FusingReification with DeBrujinFunctions.AsFunctionApp {
        outer =>
        type Language = LanguageDBSF
        def substitutor[S] = new DeBrujinFunctionsAdvanced.Substitution[S] with ShortcutFusion.IgnoreContext {
          val base: outer.type = outer
        }
      }
    }
  }

  object DirectFusionAndBetaReduceHOAS extends Lists.ToShortcutFusion with Lists.ClientInterface with HOAS.Forward {
    val base = new HOAS.ToDeBrujin with ShortcutFusion.IgnoreContext {
      val base = new DeBrujinFunctions.Reification with ShortcutFusion.FusingReification with DeBrujinFunctions.AsFunctionApp {
        type Language = LanguageDBSF
      }
    }
  }

  object BetaReduceHOAS extends HOAS.FromDeBrujin with ShortcutFusion.IgnoreContext {
    val base = new HOASAdvanced.BetaReducingReification with ShortcutFusion.Reification {
      val s: HOASFullSemantics.type = HOASFullSemantics
    }
  }
}

/**
 * Implement a small basic language interface using polymorphic embedding.
 * This language interface (in Semantics) embeds a few constructs of core Scala.
 * 
 * To make the syntax for the user resemble the corresponding Scala syntax, we
 * add ClientInterface which uses the pimp-my-library pattern to make the
 * interface conveniently available.
 * However, to make plus available only on expressions corresponding to integers,
 * we use a parametric typed representation, Rep[T], which is mapped by each
 * semantics to an appropriate representation of type T — for instance, the
 * evaluation semantics defines Rep[T] to be T.
 */
object Base {
  trait Semantics {
    type Rep[T]

    //A generic implicit conversion, lifting base values into expressions of our DSLs.
    implicit def literal[T](t: T): Rep[T]

    //More specialized alternatives to literal are conceivable:
    //implicit def literalInt(i: Int): Rep[Int]
    //implicit def literalList[T](l: List[T]): Rep[List[T]]

    def plus(a: Rep[Int], b: Rep[Int]): Rep[Int]
    def mult(a: Rep[Int], b: Rep[Int]): Rep[Int]
  }

  trait ClientInterface extends Semantics {
    implicit class IntOps(a: Rep[Int]) {
      def +(b: Rep[Int]) = plus(a, b)
      def *(b: Rep[Int]) = mult(a, b)
    }

    //Will force the compiler to insert an implicit conversion - the most
    //appropriate one - if needed.
    def asRep[T](t: Rep[T]) = t
  }

  /**
   * As in polymorphic embedding, define the AST nodes, and fold, a way to convert
   * ASTs back to polymorphically embedded programs.
   */
  object Syntax {
    trait Exp[-L <: Semantics, T] {
      def fold(semantics: L): semantics.Rep[T]
    }

    case class Const[T](t: T) extends Exp[Semantics, T] {
      def fold(semantics: Semantics): semantics.Rep[T] = semantics.literal(t)
    }

    case class Plus[L <: Semantics](a: Exp[L, Int], b: Exp[L, Int]) extends Exp[L, Int] {
      def fold(semantics: L): semantics.Rep[Int] = semantics.plus(a fold semantics, b fold semantics)
    }

    case class Mult[L <: Semantics](a: Exp[L, Int], b: Exp[L, Int]) extends Exp[L, Int] {
      def fold(semantics: L): semantics.Rep[Int] = semantics.mult(a fold semantics, b fold semantics)
    }
  }

  /**
   * Define a semantics which allows to reify programs.
   */
  trait Reification extends ClientInterface {
    type Language <: Semantics
    import Syntax._

    //This line is similar to:
    //type Regions = Exp[Language]
    type Rep[T] = Exp[Language, T]
    override implicit def literal[T](t: T) = Const(t)
    override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] = Plus(a, b)
    override def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] = Mult(a, b)
  }

  //Did we define everything or did we forget something? Let's make sure — if we
  //forgot something, we'll get an error on the next line.
  object Reification extends Reification

  //An evaluation semantics.
  trait Eval extends ClientInterface {
    type Rep[T] = T
    override implicit def literal[T](t: T) = t
    override def plus(a: Int, b: Int) = a + b
    override def mult(a: Int, b: Int) = a * b
  }

  object Eval extends Eval

  //A forwarding semantics. Becomes useful when combined with other semantics.
  trait Forward extends Semantics {
    val base: Semantics
    type Rep[T] = base.Rep[T]

    override implicit def literal[T](t: T) = base.literal(t)
    override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] = base.plus(a, b)
    override def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] = base.mult(a, b)
  }

  class ForwardConcrete(val base: Semantics) extends Forward

  //A forwarding semantics — see IgnoreContext in code
  //examples on polymorphic embedding.
  trait IgnoreContext extends ClientInterface {
    val base: Semantics
    type Context
    type Rep[T] = Context => base.Rep[T]

    override implicit def literal[T](t: T) = ctx => base.literal(t)
    override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] = ctx => base.plus(a(ctx), b(ctx))
    override def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] = ctx => base.mult(a(ctx), b(ctx))
  }

  class IgnoreContextConcrete(val base: Semantics) extends IgnoreContext
}

/*
 * A language interface for function application.
 * Language interfaces for function abstraction are more varied, hence they are
 * specified below. 
 */
object FunctionApp {
  trait Semantics extends Base.Semantics {
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T]
  }
}

//A language interface for functions, represented with de Brujin indexes.
//The auxiliary traits could be generated automatically.
object DeBrujinFunctions {
  trait Semantics extends Base.Semantics {
    /*
     * Both dbFun and dbVar are not really type-safe and are inconvenient to use,
     * because we need to specify the right type parameter
     * (S for dbFun, T for dbVar).
     */
    def dbFun[S, T](body: Rep[T]): Rep[S => T]
    def dbVar[T](idx: Int): Rep[T]

    def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T]
  }
  
  //Adapt DeBrujin 
  trait AsFunctionApp extends Semantics with FunctionApp.Semantics {
    override implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = dbApp(fun)    
  }

  trait Forward extends Semantics with Base.Forward {
    val base: Semantics

    def dbFun[S, T](body: Rep[T]): Rep[S => T] = base dbFun body
    def dbVar[T](idx: Int): Rep[T] = base dbVar idx
    def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = base dbApp fun
  }

  class ForwardConcrete(val base: Semantics) extends Forward
  
  object Syntax {
    import Base.Syntax._
    case class DBVar[T](idx: Int) extends Exp[Semantics, T] {
      //XXX: This type annotation should be inferrable, but it is not,
      //likely due to a Scalac bug.
      def fold(semantics: Semantics): semantics.Rep[T] = semantics.dbVar(idx)
    }

    case class DBApp[L <: Semantics, S, T](fun: Exp[L, S => T], arg: Exp[L, S]) extends Exp[L, T] {
      def fold(semantics: L): semantics.Rep[T] = semantics.dbApp(fun fold semantics)(arg fold semantics)
    }

    case class DBFun[L <: Semantics, S, T](body: Exp[L, T]) extends Exp[L, S => T] {
      def fold(semantics: L): semantics.Rep[S => T] = semantics.dbFun(body fold semantics)
    }
  }

  trait Reification extends Base.Reification with Semantics {
    type Language <: Semantics

    import Syntax._

    implicit def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = arg => DBApp(fun, arg)
    def dbFun[S, T](body: Rep[T]): Rep[S => T] = DBFun(body)
    def dbVar[T](idx: Int): Rep[T] = DBVar(idx)
  }

  object Reification extends Reification
}

object HOAS {
  trait Semantics extends Base.Semantics with FunctionApp.Semantics {
    implicit def fun[S, T](hoasBody: Rep[S] => Rep[T]): Rep[S => T]
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T]
  }

  trait Forward extends Semantics with Base.Forward {
    val base: Semantics
    implicit def fun[S, T](hoasBody: Rep[S] => Rep[T]): Rep[S => T] = base.fun(hoasBody)
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = base.app(fun)
  }

  //Conversion from and to de Brujin indexes.
  //This code reuses the same ideas as the conversion seen in the previous lecture.

  trait ToDeBrujin extends Semantics with Base.IgnoreContext {
    type Context = Int

    val base: DeBrujinFunctions.Semantics

    override implicit def fun[S, T](hoasBody: Rep[S] => Rep[T]): Rep[S => T] =
      ctxDepth => base.dbFun(hoasBody(nestedCtxDepth => base.dbVar(nestedCtxDepth - (ctxDepth + 1)))(ctxDepth + 1))

    override implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      arg => ctxDepth => base.dbApp(fun(ctxDepth))(arg(ctxDepth))
  }

  class ToDeBrujinC(val base: DeBrujinFunctions.Semantics) extends ToDeBrujin

  trait FromDeBrujin extends DeBrujinFunctions.Semantics with Base.IgnoreContext {
    type Context = List[base.Rep[_]]
    val base: HOAS.Semantics

    override def dbVar[T](idx: Int): Rep[T] = ctx => ctx(idx).asInstanceOf[base.Rep[T]]
    override def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      arg => ctx => base.app(fun(ctx))(arg(ctx))
    override def dbFun[S, T](body: Rep[T]): Rep[S => T] =
      ctx => base.fun(arg => body(arg :: ctx))
  }
  class FromDeBrujinC(val base: HOAS.Semantics) extends FromDeBrujin
  
  //Implementing reification for HOAS is rather hard, so it is only shown in Advanced.scala
}

/**
 * A minimal language interface for operations on list.
 */
object Lists {
  trait Semantics extends Base.Semantics {
    def list_map[T, U](l: Rep[List[T]], f: Rep[T => U]): Rep[List[U]]
  }

  trait ClientInterface extends Semantics with HOAS.Semantics with Base.ClientInterface {
    implicit class ListOps[T](l: Rep[List[T]]) {
      def map[U](f: Rep[T] => Rep[U]) = list_map(l, f) // Notice the implicit conversion used here on f!
    }
  }

  object Syntax {
    import Base.Syntax._

    case class ListMap[L <: Semantics, T, U](list: Exp[L, List[T]], fun: Exp[L, T => U]) extends Exp[L, List[U]] {
      def fold(semantics: L): semantics.Rep[List[U]] = semantics.list_map(list fold semantics, fun fold semantics)
    }
  }

  trait Reification extends Base.Reification with Semantics {
    type Language <: Semantics
    import Syntax._
    override def list_map[T, U](l: Rep[List[T]], f: Rep[T => U]): Rep[List[U]] = ListMap(l, f)
  }
  object Reification extends Reification

  trait ToShortcutFusion extends Semantics with Base.Forward {
    val base: HOAS.Semantics with ShortcutFusion.Semantics
    def list_map[T, U](l: Rep[List[T]], f: Rep[T => U]): Rep[List[U]] = {
      import base._
      build(fun(zero =>
        fun(cons =>
          foldRight(l, zero)
              (fun(hd => fun(tl => cons(f(hd))(tl)))))))
    }
  }

  //This constructor induces a function, which is then a first-class value. However, that's often not enough, because we need to also mix this trait with other ones.
  class ToShortcutFusionC(val base: HOAS.Semantics with ShortcutFusion.Semantics) extends ToShortcutFusion

  object ToShortcutFusion extends ToShortcutFusion with ClientInterface with HOAS.Forward {
    val base = new HOAS.ToDeBrujin with ShortcutFusion.IgnoreContext {
      val base = new DeBrujinFunctions.Reification with ShortcutFusion.Reification {
        type Language = DeBrujinFunctions.Semantics with ShortcutFusion.Semantics
      }
    }
  }
}

/**
 * A language interface for a minimal set of primitive operations on lists.
 * These operations allow an optimization known as "shortcut fusion".
 * This is described in the paper "A shortcut to deforestation".
 */
object ShortcutFusion {
  trait Semantics extends Base.Semantics {
    def foldRight[T, U](l: Rep[List[T]], zero: Rep[U])(op: Rep[T => U => U]): Rep[U]
    def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]]
  }

  object Syntax {
    import Base.Syntax._

    case class FoldRight[L <: Semantics, T, U](listExp: Exp[L, List[T]], zero: Exp[L, U], op: Exp[L, T => U => U]) extends Exp[L, U] {
      def fold(semantics: L): semantics.Rep[U] = semantics.foldRight(listExp fold semantics, zero fold semantics)(op fold semantics)
    }
    //We make Build get a specific variant of a Church-encoded list. Instead, the correct definition of build takes a polymorphic function as argument. Hence, this is less safe.
    case class Build[L <: Semantics, T](churchEncodedList: Exp[L, List[T] => (T => List[T] => List[T]) => List[T]]) extends Exp[L, List[T]] {
      def fold(semantics: L): semantics.Rep[List[T]] = semantics.build(churchEncodedList fold semantics)
    }
  }

  trait Forward extends Base.Forward with Semantics {
    val base: Semantics

    override def foldRight[T, U](l: Rep[List[T]], zero: Rep[U])(op: Rep[T => U => U]): Rep[U] =
      base.foldRight(l, zero)(op)
    override def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]] =
      base.build(churchEncodedList)
  }

  trait IgnoreContext extends Base.IgnoreContext with Semantics {
    val base: Semantics

    override def foldRight[T, U](l: Rep[List[T]], zero: Rep[U])(op: Rep[T => U => U]): Rep[U] =
      ctx => base.foldRight(l(ctx), zero(ctx))(op(ctx))
    override def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]] =
      ctx => base.build(churchEncodedList(ctx))
  }

  class IgnoreContextConcrete(val base: Semantics) extends IgnoreContext

  trait Reification extends Base.Reification with Semantics {
    type Language <: Semantics
    import Syntax._

    def foldRight[T, U](l: Rep[List[T]], zero: Rep[U])(op: Rep[T => U => U]): Rep[U] = FoldRight(l, zero, op)
    def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]] = Build(churchEncodedList)
  }
  object Reification extends Reification

  trait FusingReification extends Reification {
    this: FunctionApp.Semantics =>

    import Syntax._
    override def foldRight[T, U](l: Rep[List[T]], zero: Rep[U])(op: Rep[T => U => U]): Rep[U] = {
      l match {
        case Build(f) =>
          //f(zero)(op) //Code one should write — that's what matters.

          //Code one has to write:
          //Since the argument of Build has the wrong type, and it is not polymorphic enough, we need typecasts here.
          //They are lies — zero does not have type Rep[U] — but at runtime these typecasts have no effect here due to erasure,
          //and the final result is typecast to the correct type (otherwise we'd get problems later).
          f(zero.asInstanceOf[Rep[List[T]]])(op.asInstanceOf[Rep[T => List[T] => List[T]]]).asInstanceOf[Rep[U]]
        case _ => super.foldRight(l, zero)(op)
      }
    }
  }
  //object FusingReification extends FusingReification
}
