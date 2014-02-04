package lecture16
import scala.language.{ implicitConversions, higherKinds }
import scala.collection.mutable

//LMS Lightweight modular staging.

//For each feature, we have a X trait defining its interface, and a XExp trait defining its implementation in terms of expressions.
trait Base {
  type Rep[T]
}

trait BaseExp extends Base {
  type Rep[T] = Exp[T]

  sealed trait Exp[T]
  case class Const[T](t: T) extends Exp[T]
  case class Sym[T](idx: Int) extends Exp[T]

  trait Def[T]

  object Def {
    def unapply[T](s: Exp[T]): Option[Def[T]] = s match {
      case s: Sym[T] => findDefinition(s)
      case _ => None
    }
  }

  var cnt: Int = 0
  def fresh: Int = {
    cnt += 1
    cnt
  }
  def freshSym[T] = Sym[T](fresh)
  
  //We can do common subexpression elimination (CSE). To do this, we need to
  //Def[T] to Exp[T] so that the same subexpression — that is, the same Def[T] —
  //maps to the same Exp[T].
  implicit def toAtom[T](d: Def[T]): Exp[T] =
    findOrCreateDefinition(d)

  //Not shown in the lecture — we need to use in fact two hash maps, to
  //implement mappings in both directions.
  private val symMap: mutable.HashMap[Sym[_], Def[_]] = new mutable.HashMap()
  private val defMap: mutable.HashMap[Def[_], Sym[_]] = new mutable.HashMap()

  //Implementation note:
  //I use a type ascription to get a type error if the type of the expression later changes.
  //So I know the typecast isn't casting between exceedingly different types.
  def findDefinition[T](s: Sym[T]): Option[Def[T]] = (symMap get s: Option[Def[_]]).asInstanceOf[Option[Def[T]]]
  def findDefinition[T](d: Def[T]): Option[Sym[T]] = (defMap get d: Option[Sym[_]]).asInstanceOf[Option[Sym[T]]]
  def findOrCreateDefinition[T](d: Def[T]): Sym[T] = {
    (defMap.getOrElseUpdate(d, {
      //Create a new symbol, and add a reverse mapping
      val s = freshSym.asInstanceOf[Sym[_]]
      symMap += s -> d
      s
    }): Sym[_]).asInstanceOf[Sym[T]]
  }
}

trait Convs extends Base {
  implicit def pure[T](t: T): Rep[T]
}

trait ConvsExp extends Convs with BaseExp {
  override implicit def pure[T](t: T): Exp[T] = Const(t)
}

trait Integers extends Base {
  def plus(a: Rep[Int], b: Rep[Int]): Rep[Int]
  def mult(a: Rep[Int], b: Rep[Int]): Rep[Int]

  //implicit magic
  implicit class IntOps(a: Rep[Int]) {
    def +(b: Rep[Int]) = plus(a, b)
    def *(b: Rep[Int]) = mult(a, b)
  }
}

trait IntegersExp extends Integers with BaseExp {
  case class Plus(a: Rep[Int], b: Rep[Int]) extends Def[Int]
  case class Mult(a: Rep[Int], b: Rep[Int]) extends Def[Int]
  def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] =
    Plus(a, b)
  def mult(a: Rep[Int], b: Rep[Int]): Rep[Int] =
    Mult(a, b)

  //HOAS (higher-order abstract syntax) function representation 
  case class HOASFun[S, T](fun: Exp[S] => Exp[T])
  //FOAS (first-order abstract syntax) representation
  case class FOASFun[S, T](x: Sym[S], body: Exp[T])

  //Conversion:
  def toFOAS[S, T](f: HOASFun[S, T]) = {
    val v = freshSym[S]
    FOASFun(v, f.fun(v))
  }

  //Barendregt convention: all symbols are distinct.

/*  
Typical problem with the Barendregt convention: assume
  f: FOASFun
  e contains f
  g is a function, you want to do inlining on g(e), and g uses its argument more than once.
Now g(e), after inlining (that is, in fact, beta-reduction) reduces to a value
which contains multiple copies of e, thus of f, so the produced term violates
the Barendregt convention.

However this is no concern here, because 
*/
}

//I can define optimizations by overriding:
trait BettersIntegerExps extends IntegersExp {
  class Foo(a: Rep[Int], b: Rep[Int]) extends Plus(a, b) with Exp[Int]

  /*
  def plusp(a: Rep[Int], b: Rep[Int]): Rep[Int] =
    a match {
      case Plus(c, d) => ???
    }
    */

  override def plus(a: Rep[Int], b: Rep[Int]): Rep[Int] =
    (a, b) match {
      case (Const(i), Const(j)) => Const(i + j)
      //Delegates to the base case:
      case (Def(Plus(a, b)), c) => ???
      case _ => super.plus(a, b)
    }
}

trait MyLanguage extends Base with Integers with Convs

trait Program extends MyLanguage {
  def power(base: Rep[Int], exp: Int): Rep[Int] = {
    if (exp == 1)
      base
    else
      power(base, exp - 1) * base
    //Scala Virtualized
  }
}

trait MyLanguageExp extends BaseExp with IntegersExp with ConvsExp

object ProgramObj extends MyLanguage with MyLanguageExp with Program with App {
  println(power(5, 3))
  //Result:
  //Sym(2,Mult( Sym(1,Mult(Const(5),Const(5))), Const(5) ))
  //that is,
  //5 * 5 * 5
  //FFT
}

/*
object L1 extends BaseExp
object L2 extends BaseExp
L1.Exp[Int] != L2.Exp[Int]
*/