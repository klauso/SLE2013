package lecture15
import scala.language.{ higherKinds, implicitConversions, postfixOps }

trait HelpersForExercises {
  import HOAS._
  trait Eval extends Base.Eval with Semantics {
    //Here, Rep[T] = T. So the signatures of fun and app become specialized
    //versions of the identity function.
    override def fun[S, T](f: S => T): S => T = f
    override def app[S, T](f: S => T): S => T = f
  }
  object Eval extends Eval
}

/**
 * Advanced material which I used in the material, but which I did not have time to
 * explain in detail. Understanding this is optional.
 */

object DeBrujinFunctionsAdvanced {
  import DeBrujinFunctions._

  /**
   * This semantics refines reification to perform a code transformation —
   * beta-reduction. To this end, we need an implementation of substitution,
   * which is shown below.
   */
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

    def dbApp[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      arg => ctx => base.dbApp(fun(ctx))(arg(ctx))

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


object HOASAdvanced {
  import HOAS._

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
    case class Fun[L <: Semantics, S, T](hoasBody: Exp[L, S] => Exp[L, T]) extends Exp[L, S => T] {
      override def toString = {
        val name = freshName()
        case class Var[T](x: String) extends FakeExp[L, T]
        s"${name} => ${hoasBody(Var[S](name)).toString}"
      }
      def fold(semantics: L): semantics.Rep[S => T] = {
        //The inverse of the fold function — following the "Boxes go bananas" paper.
        //But note we need the cast!
        case class Place[T](arg: semantics.Rep[T]) extends Exp[L, T] {
          override def fold(semantics2: L): semantics2.Rep[T] =
            //Does not help
            /*
            semantics2 match {
              case _: semantics.type @unchecked => arg.asInstanceOf[semantics2.Rep[T]]
            }
            */
            arg.asInstanceOf[semantics2.Rep[T]]
        }
        //We can define a more precise version...
        case class Place2[T](arg: semantics.Rep[T]) extends Exp[semantics.type, T] {
          override def fold(semantics2: semantics.type): semantics2.Rep[T] =
            arg
        }
        //But this does not work, because Exp is contravariant — see the compile error here:
        //semantics.fun[S, T](arg => hoasBody(Place2(arg)) fold semantics)
        //Consider changing the Language member, which is a covariant type member!
        semantics.fun[S, T](arg => hoasBody(Place(arg)) fold semantics)
      }
    }
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] =
      App(fun, _)

    implicit def fun[S, T](body: Rep[S] => Rep[T]): Rep[S => T] = Fun(body)
  }

  // To make boxes go bananas work, you need to specify the semantics for later folding before you start reifying the trait.
  trait ReificationPerfect extends Base.Reification with Semantics {
    //type Language = Semantics with Singleton//s.type
    val s: Semantics
    type Language = s.type

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
    case class Fun[S, T](hoasBody: Exp[Language, S] => Exp[Language, T]) extends Exp[Language, S => T] {
      override def toString = {
        val name = freshName()
        case class Var[T](x: String) extends FakeExp[Language, T]
        s"${name} => ${hoasBody(Var[S](name)).toString}"
      }
      def fold(semantics: Language): semantics.Rep[S => T] = {
        //The inverse of the fold function — following the "Boxes go bananas" paper.
        //And here, we don't need the cast any more!
        case class Place[T](arg: semantics.Rep[T]) extends Exp[Language, T] {
          override def fold(semantics2: Language): semantics2.Rep[T] =
            arg
        }

        //But this does not work, because Exp is contravariant — see the compile error here:
        semantics.fun[S, T](arg => hoasBody(Place(arg)) fold semantics)
        //Consider changing the Language member, which is a covariant type member!
        //semantics.fun[S, T](arg => hoasBody(Place(arg)) fold semantics)
      }
    }
    implicit def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = {
      App(fun, _)
    }

    implicit def fun[S, T](body: Rep[S] => Rep[T]): Rep[S => T] = Fun(body)
  }

  trait BetaReducingReification extends ReificationPerfect {
    import Base.Syntax._
    implicit override def app[S, T](fun: Rep[S => T]): Rep[S] => Rep[T] = {
      //Recognize opportunities for beta-reduction.
      fun match {
        case Fun(body) => body
        case _ => super.app(fun)
      }
    }
  }
}
