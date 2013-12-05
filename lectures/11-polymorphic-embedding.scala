import scala.language.implicitConversions
import scala.language.higherKinds

/* 
=============================
Polymorphic Embedding of DSLs
=============================

based on  Christian Hofer, Klaus Ostermann, Tillmann Rendel, Adriaan Moors: 
Polymorphic embedding of dsls. GPCE 2008: 137-148
http://www.mathematik.uni-marburg.de/~rendel/hofer08polymorphic.pdf
*/


trait Regions {
  type Vector = (Double,Double)
  
  type Region
  
  def univ: Region
  def empty : Region
  def circle : Region
  def scale(v: Vector, x: Region) : Region
  def union(x: Region, y: Region) : Region
}

def program(semantics: Regions) : semantics.Region = {
  import semantics._
  val ellipse24 = scale((2,4), circle)
  union(univ, ellipse24)
}  

trait Evaluation extends Regions {
  type Region = Vector => Boolean
  def univ = p => true
  def empty = p => false
  def circle = p => p._1 * p._1 + p._2 * p._2 < 1
  def scale(v: Vector, x: Region) = p => x(p._1 /v._1, p._2/v._2)
  def union(x: Region, y: Region) = p => x(p) || y(p)
}  

object Eval extends Evaluation

assert(program(Eval)((1,2)) == true)

trait Printing extends Regions {
  type Region = String
  def univ = "univ"
  def empty = "empty"
  def circle = "circle"
  def scale(v: Vector, x: Region) = "scale("+v+", "+x+")"
  def union(x: Region, y: Region) = "union(" + x + ", "+ y + ")"
}

object Print extends Printing

assert(program(Print) == "union(univ, scale((2.0,4.0), circle))")

trait Optimization extends Regions {
  val semantics : Regions
  
  type Region = (semantics.Region, Boolean)
  def univ = (semantics.univ, true)
  def empty = (semantics.empty, false)
  def circle = (semantics.circle, false)
  def scale(v: Vector, x: Region) =
    if (x._2) (semantics.univ, true) else (semantics.scale(v,x._1), false)
  def union(x: Region, y: Region) = 
  if (x._2 || y._2) (semantics.univ, true) else (semantics.union(x._1, y._1), false)
}


assert(program( new Optimization { val semantics = Print })._1 == "univ")
  

/* Hierarchical composition sketch:
trait Regions {
  val vec: Vecctors
  import vec._
  ...
}

trait Optimization extends Regions {
  val semantics : Regions
  val vec : semantics.vec.type = semantics.vec
  import vec._
  ...
}
*/
  
trait Functions {
  type Rep[X]
  def fun[S,T](f: Rep[S] => Rep[T]) : Rep[S=>T]
  def app[S,T](f: Rep[S=>T], v: Rep[S]) : Rep[T]
}  

trait FunEval extends Functions {
  type Rep[T] = T
  def fun[S,T](f: S=>T) = f
  def app[S,T](f: S=>T, v: S) = f(v)
}

trait FunPrinting extends Functions {
  var varcounter = 0
  def nextVariable : String = {
    varcounter += 1
    "x"+varcounter
  }
  type Rep[X] = String
  def fun[S,T](f: String => String) : String = {
    val v = nextVariable
    "fun("+v+" => "+f(v)+")"
  }
  def app[S,T](f: String, v: String) = f+"("+v+")"
}

trait FunReg extends Regions with Functions {
  implicit def fromRegion(r: Region) : Rep[Region]
  implicit def toRegion(r: Rep[Region]): Region
}

def program(semantics: FunReg) : semantics.Rep[semantics.Region] = {
  import semantics._
  app(fun[Region,Region]( x => scale((5,2),x)), empty)  // where does the compiler insert calls
                                                        // to fromRegion and toRegion here?
}  
  
object FunRegEval extends FunReg with Evaluation with FunEval {
  implicit def fromRegion(r: Region): Rep[Region] = r
  implicit def toRegion(r: Rep[Region]) : Region = r
}  

object FunRegPrinting extends FunReg with Printing with FunPrinting {
  implicit def fromRegion(r: Region) : Rep[Region] = r
  implicit def toRegion(r: Rep[Region]): Region = r
}

assert(program(FunRegPrinting) == "fun(x1 => scale((5.0,2.0), x1))(empty)")

assert(program(FunRegEval)(2,3) == false)

