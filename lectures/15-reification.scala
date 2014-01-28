// Tested with Scala 2.10.3

package lecture15
import scala.language.{ higherKinds, implicitConversions, postfixOps }
/*
val l0 = List(1, 2, 3, ..., 1000 * 1000) map (l0 => l0 + 1)
l0 map (x => x * 2)

l0 map f map g => l0 map (f andThen g)

(f andThen g)(l0) = g(f(l0))

val l1 = if (cond) l0 map f else ...
fuse(l1 map g)
val l0 = asRep(List(1, 2, 3, ...))
*/

object Main extends scala.App {
  def prog1(semantics: Base.Semantics) = {
    import semantics._
    asRep(1) + 2
  }
  println(prog1(Base.Eval))
  println(prog1(Base.Reification))

  println(prog1(DeBrujin.Reification))

  def listExp1(semantics: Lists.Semantics) = {
    import semantics._
    asRep(List(1, 2, 3)) map (x => x + 1)
  }
  def listExp2(semantics: Lists.Semantics) = {
    import semantics._
    listExp1(semantics) map (x => x * 2)
  }
  def listExp3(semantics: Lists.Semantics) = {
    import semantics._
    listExp2(semantics) map (_ + 2)
  }

  import Base.Syntax.Exp
  def fusion[T](e: Exp[ShortcutFusion.Semantics with DeBrujin.Semantics, T]) =
    e fold (new DeBrujin.Reification with ShortcutFusion.FusingReification {
      type Language = DeBrujin.Semantics with ShortcutFusion.Semantics
    })

  def betaReduce[T](e: Exp[ShortcutFusion.Semantics with DeBrujin.Semantics, T]) =
    e fold (new DeBrujin.BetaReducingReification with ShortcutFusion.Reification {
      outer =>
      type Language = DeBrujin.Semantics with ShortcutFusion.Semantics
      def substitutor[S] = new DeBrujin.Substitution[S] with ShortcutFusion.IgnoreContext {
        val base: outer.type = outer
      }
    })

  def toHoas[T](e: Exp[ShortcutFusion.Semantics with DeBrujin.Semantics, T]) =
    (e fold (new HOAS.FromDeBrujin with ShortcutFusion.IgnoreContext {
      val base = new HOAS.Reification with ShortcutFusion.Reification {
        type Language = HOAS.Semantics with ShortcutFusion.Semantics
      }
    }))(Nil)

  def trace[T](e: Exp[ShortcutFusion.Semantics with DeBrujin.Semantics, T]) = {
    println()
    println(e)
    println(toHoas(e))
  }

  //val beforeFusion = listExp3(Lists.ToShortcutFusion)(0)
  val beforeFusion = listExp2(Lists.ToShortcutFusion)(0)
  trace(beforeFusion)
  val afterFusion = fusion(beforeFusion)
  trace(afterFusion)
  val betaReduced = betaReduce(afterFusion)
  trace(betaReduced)
}

/*
Build(x_1 => cons =>
  FoldRight(Const(List(1, 2, 3)),
      Var(x_1),
      hd => tl =>
        cons((hd + 1) * 2, tl)))

      x_3 => x_4 => App(App(Var(x_2),
          Mult(Plus(Var(x_3),Const(1)),Const(2))),Var(x_4))))
*/
/*
 *
 */
object Base {
  trait Semantics {
    type Rep[T]

    implicit def literal[T](t: T): Rep[T]
    def plus(a: Rep[Int], b: Rep[Int]): Rep[Int]
    def mult(a: Rep[Int], b: Rep[Int]): Rep[Int]

    implicit class IntOps(a: Rep[Int]) {
      def +(b: Rep[Int]) = plus(a, b)
      def *(b: Rep[Int]) = mult(a, b)
    }
    def asRep[T](t: Rep[T]) = t
  }

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

  trait Reification extends Semantics {
    type Language <: Semantics
    import Syntax._

    type Rep[T] = Exp[Language, T]
    override implicit def literal[T](t: T) = Const(t)
    override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] = Plus(a, b)
    override def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] = Mult(a, b)
  }

  object Reification extends Reification

  trait Eval extends Semantics {
    type Rep[T] = T
    override implicit def literal[T](t: T) = t
    override def plus(a: Int, b: Int) = a + b
    override def mult(a: Int, b: Int) = a * b
  }

  object Eval extends Eval

  trait IgnoreContext extends Semantics {
    val base: Semantics
    type Context
    type Rep[T] = Context => base.Rep[T]

    override implicit def literal[T](t: T) = ctx => base.literal(t)
    override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] = ctx => base.plus(a(ctx), b(ctx))
    override def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] = ctx => base.mult(a(ctx), b(ctx))
  }

  trait Forward extends Semantics {
    val base: Semantics
    type Rep[T] = base.Rep[T]

    override implicit def literal[T](t: T) = base.literal(t)
    override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] = base.plus(a, b)
    override def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] = base.mult(a, b)
  }

  class IgnoreContextConcrete(val base: Semantics) extends IgnoreContext
}

trait FunctionApp extends Base.Semantics {
  implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T]
}

object DeBrujin {
  trait Semantics extends Base.Semantics with FunctionApp {
    def dbFun[S, T](body: Rep[T]): Rep[S => T]
    def dbVar[T](idx: Int): Rep[T]
    def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T]
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = dbApp(fun)
  }

  object Syntax {
    import Base.Syntax._
    case class DBVar[T](idx: Int) extends Exp[Semantics, T] {
      //(XXX: Paolo, try removing the type annotation: then it does not compile)
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
  trait BetaReducingReification extends Reification {
    outer =>
    import Syntax._

    override def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      arg => {
        fun match {
          case DBFun(body) => subst(arg)(body)
          case _ => super.dbApp(fun)(arg)
        }
      }
    def subst[S, T](arg: Rep[S])(term: Rep[T]): Rep[T] = (term fold substitutor[S])((0, arg))
    def substitutor[S]: Substitution[S] with Language {
      val base: outer.type
    }
  }
  trait Substitution[S] extends Semantics with Base.IgnoreContext {
    type Context = (Int, base.Rep[S])
    val base: Semantics

    import Syntax._

    def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = arg => ctx => base.dbApp(fun(ctx))(arg(ctx))
    def dbFun[S, T](body: Rep[T]): Rep[S => T] = {
      case (depth, arg) =>
        base.dbFun(body((depth + 1, arg)))
    }
    def dbVar[T](idx: Int): Rep[T] = {
      case (depth, arg) =>
        if (idx == depth)
          arg.asInstanceOf[base.Rep[T]] //Here S and T should match
        else
          base.dbVar[T](idx)
    }
  }
}

object HOAS {
  trait Semantics extends Base.Semantics with FunctionApp {
    implicit def fun[S, T](hoasBody: Rep[S] => Rep[T]): Rep[S => T]
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T]
  }

  trait Reification extends Base.Reification with Semantics {
    type Language <: Semantics

    import Base.Syntax._
    val freshName = {
      var cnt = 0
      () => {
        cnt += 1
        "x_" + cnt
      }
    }
    //This is not a foldable AST. But it is still useful for pretty-printing.

    class FakeExp[-L <: Semantics, T] extends Exp[L, T] {
      def fold(semantics: L): semantics.Rep[T] = ???
    }
    case class App[L <: Semantics, S, T](fun: Exp[L, S => T], arg: Exp[L, S]) extends Exp[L, T] {
      def fold(semantics: L): semantics.Rep[T] = semantics.app(fun fold semantics)(arg fold semantics)
    }
    //XXX, L is in a contravariant position here...
    case class Fun[L <: Semantics, S, T](hoasBody: Exp[L, S] => Exp[L, T]) extends Exp[L, S => T] {
      override def toString = {
        val name = freshName()
        case class Var[T](x: String) extends FakeExp[L, T]
        s"${name} => ${hoasBody(Var[S](name)).toString}"
      }
      def fold(semantics: L): semantics.Rep[S => T] = {
        //The inverse of the fold function — following the "Boxes go bananas" paper.
        case class Place[T](arg: semantics.Rep[T]) extends Exp[L, T] {
          override def fold(semantics2: L): semantics2.Rep[T] =
            semantics2 match {
              case _: semantics.type @unchecked => arg.asInstanceOf[semantics2.Rep[T]]
            }
        }
        semantics.fun[S, T](arg => hoasBody(Place(arg)) fold semantics)
      }
    }
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = {
      //Simple implementation:
      App(fun, _)
      //Instead, opportunistically try to do inlining, if enabled:
      /*fun match {
        case Fun(body) if doInlining => body
        case _ => App(fun, _)
      }*/
    }

    implicit def fun[S, T](body: Rep[S] => Rep[T]): Rep[S => T] = Fun(body)
  }

  trait Forward extends Semantics with Base.Forward {
    val base: Semantics
    implicit def fun[S, T](hoasBody: Rep[S] => Rep[T]): Rep[S => T] = base.fun(hoasBody)
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = base.app(fun)
  }

  trait ToDeBrujin extends Semantics with Base.IgnoreContext {
    type Context = Int

    val base: DeBrujin.Semantics

    override implicit def fun[S, T](hoasBody: Rep[S] => Rep[T]): Rep[S => T] =
      ctxDepth => base.dbFun(hoasBody(nestedCtxDepth => base.dbVar(nestedCtxDepth - (ctxDepth + 1)))(ctxDepth + 1))

    override implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      arg => ctxDepth => base.dbApp(fun(ctxDepth))(arg(ctxDepth))
  }

  class ToDeBrujinC(val base: DeBrujin.Semantics) extends ToDeBrujin

  trait FromDeBrujin extends DeBrujin.Semantics with Base.IgnoreContext {
    type Context = List[base.Rep[_]]
    val base: HOAS.Semantics

    override def dbVar[T](idx: Int): Rep[T] = ctx => ctx(idx).asInstanceOf[base.Rep[T]]
    override def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      arg => ctx => base.app(fun(ctx))(arg(ctx))
    override def dbFun[S, T](body: Rep[T]): Rep[S => T] =
      ctx => base.fun(arg => body(arg :: ctx))
  }
  class FromDeBrujinC(val base: HOAS.Semantics) extends FromDeBrujin
}

object Lists {
  trait Semantics extends HOAS.Semantics {
    def list_map[T, U](l: Rep[List[T]], f: Rep[T => U]): Rep[List[U]]

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

  trait Reification extends Base.Reification {
    type Language <: Semantics
    import Syntax._
    def list_map[T, U](l: Rep[List[T]], f: Rep[T => U]): Rep[List[U]] = ListMap(l, f)
  }
  object Reification extends Reification

  trait ToShortcutFusion extends Semantics with HOAS.Forward with ShortcutFusion.Forward {
    val base: HOAS.Semantics with ShortcutFusion.Semantics
    def list_map[T, U](l: Rep[List[T]], f: Rep[T => U]): Rep[List[U]] = {
      base.build[U](fun(zero =>
        fun(cons =>
          base.foldRight[T, List[U]](l, zero,
              fun(hd => fun(tl => cons(f(hd))(tl)))))))
    }
  }
  class ToShortcutFusionC(val base: HOAS.Semantics with ShortcutFusion.Semantics) extends ToShortcutFusion
  object ToShortcutFusion extends ToShortcutFusion {
    val base = new HOAS.ToDeBrujin with ShortcutFusion.IgnoreContext {
      val base = new DeBrujin.Reification with ShortcutFusion.Reification {
        type Language = DeBrujin.Semantics with ShortcutFusion.Semantics
      }
    }
  }
}

object ShortcutFusion {
  trait Semantics extends Base.Semantics {
    def foldRight[T, U](l: Rep[List[T]], zero: Rep[U], op: Rep[T => U => U]): Rep[U]
    def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]]
  }

  object Syntax {
    import Base.Syntax._

    case class FoldRight[L <: Semantics, T, U](listExp: Exp[L, List[T]], zero: Exp[L, U], op: Exp[L, T => U => U]) extends Exp[L, U] {
      def fold(semantics: L): semantics.Rep[U] = semantics.foldRight(listExp fold semantics, zero fold semantics, op fold semantics)
    }
    //We make Build get a specific variant of a Church-encoded list. Instead, the correct definition of build takes a polymorphic function as argument. Hence, this is less safe.
    case class Build[L <: Semantics, T](churchEncodedList: Exp[L, List[T] => (T => List[T] => List[T]) => List[T]]) extends Exp[L, List[T]] {
      def fold(semantics: L): semantics.Rep[List[T]] = semantics.build(churchEncodedList fold semantics)
    }
  }

  trait Forward extends Base.Forward with Semantics {
    val base: Semantics

    override def foldRight[T, U](l: Rep[List[T]], zero: Rep[U], op: Rep[T => U => U]): Rep[U] =
      base.foldRight(l, zero, op)
    override def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]] =
      base.build(churchEncodedList)
  }

  trait IgnoreContext extends Base.IgnoreContext with Semantics {
    val base: Semantics

    override def foldRight[T, U](l: Rep[List[T]], zero: Rep[U], op: Rep[T => U => U]): Rep[U] =
      ctx => base.foldRight(l(ctx), zero(ctx), op(ctx))
    override def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]] =
      ctx => base.build(churchEncodedList(ctx))
  }

  class IgnoreContextConcrete(val base: Semantics) extends IgnoreContext

  trait Reification extends Base.Reification with Semantics {
    type Language <: Semantics
    import Syntax._

    def foldRight[T, U](l: Rep[List[T]], zero: Rep[U], op: Rep[T => U => U]): Rep[U] = FoldRight(l, zero, op)
    def build[T](churchEncodedList: Rep[List[T] => (T => List[T] => List[T]) => List[T]]): Rep[List[T]] = Build(churchEncodedList)
  }
  object Reification extends Reification

  trait FusingReification extends Reification {
    this: FunctionApp =>

    import Syntax._
    override def foldRight[T, U](l: Rep[List[T]], zero: Rep[U], op: Rep[T => U => U]): Rep[U] = {
      l match {
        //case b: Build[_, t] =>
          //b.churchEncodedList(zero)(op)
        case Build(f) =>
          //f(zero)(op) //Code one should write
          //Code one has to write:
          //Since the argument of Build is not polymorphic enough, we need typecasts here.
          f(zero.asInstanceOf[Rep[List[T]]])(op.asInstanceOf[Rep[T => List[T] => List[T]]]).asInstanceOf[Rep[U]]
        case _ => super.foldRight(l, zero, op)
      }
    }
  }
  //object FusingReification extends FusingReification
}
