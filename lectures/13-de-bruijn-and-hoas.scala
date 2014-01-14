object Lecture13 extends App {
  // --------------------------
  // THE BASIC REGIONS LANGUAGE
  // --------------------------

  object Regions {
    // Language Interface
    trait Semantics {
      type Vector = (Double, Double)
      type Region

      def univ: Region
      def empty: Region
      def circle: Region
      def scale(v: Vector, r: Region): Region
      def union(r: Region, s: Region): Region
      def translate(v: Vector, r: Region): Region
    }

    // Syntax
    object Syntax {
      type Vector = (Double, Double)
      trait Region[-L <: Semantics] {
        def fold(semantics: L): semantics.Region
      }

      case object Univ extends Region[Semantics] {
        def fold(semantics: Semantics): semantics.Region = semantics.univ
      }

      case object Empty extends Region[Semantics] {
        def fold(semantics: Semantics): semantics.Region = semantics.empty
      }

      case object Circle extends Region[Semantics] {
        def fold(semantics: Semantics): semantics.Region = semantics.circle
      }

      case class Scale[L <: Semantics](v: Vector, r: Region[L]) extends Region[L] {
        def fold(semantics: L): semantics.Region = semantics.scale(v, r.fold(semantics))
      }

      case class Union[L <: Semantics](r: Region[L], s: Region[L]) extends Region[L] {
        def fold(semantics: L): semantics.Region = semantics.union(r.fold(semantics), s.fold(semantics))
      }

      case class Translate[L <: Semantics](v: Vector, r: Region[L]) extends Region[L] {
        def fold(semantics: L): semantics.Region = semantics.translate(v, r.fold(semantics))
      }
    }

    // Reification semantics
    trait Reification extends Semantics {
      type Language <: Semantics
      type Region = Syntax.Region[Language]

      def univ: Region = Syntax.Univ
      def empty: Region = Syntax.Empty
      def circle: Region = Syntax.Circle
      def scale(v: Vector, r: Region): Region = Syntax.Scale(v, r)
      def union(r: Region, s: Region): Region = Syntax.Union(r, s)
      def translate(v: Vector, r: Region): Region = Syntax.Translate(v, r)
    }

    object Reification extends Reification

    // Evaluation semantics
    trait PointInRegion extends Semantics {
      type Region = (Double, Double) => Boolean

      def univ: Region =
        (x, y) => true
      def empty: Region =
        (x, y) => false
      def circle: Region =
        (x, y) => x * x + y * y <= 1
      def scale(v: Vector, r: Region): Region =
        (x, y) => r(x / v._1, y / v._1)
      def union(r: Region, s: Region): Region =
        (x, y) => r(x, y) || s(x, y)
      def translate(v: Vector, r: Region): Region =
        (x, y) => r(x - v._1, y - v._2)
    }

    object PointInRegion extends PointInRegion

    // Forwarding semantics (ignoring some context)
    trait IgnoreContext extends Semantics {
      val base: Semantics
      type Context

      type Region = Context => base.Region

      def univ: Region =
        context => base.univ
      def empty: Region =
        context => base.empty
      def circle: Region =
        context => base.circle
      def scale(v: Vector, x: Region): Region =
        context => base.scale(v, x(context))
      def union(x: Region, y: Region): Region =
        context => base.union(x(context), y(context))
      def translate(v: Vector, x: Region): Region =
        context => base.translate(v, x(context))
    }
  }

  // -----------------------
  // DE BRUIJN BASED SHARING
  // -----------------------

  object DeBruijn {
    // De Bruijn interface of sharing
    trait Semantics extends Regions.Semantics {
      def let(value: Region, body: Region): Region
      def ref(index: Int): Region
    }

    // Syntax
    object Syntax {
      import Regions.Syntax._

      case class Let[L <: Semantics](value: Region[L], body: Region[L]) extends Region[L] {
        def fold(semantics: L): semantics.Region = semantics.let(value.fold(semantics), body.fold(semantics))
      }

      case class Ref(index: Int) extends Region[Semantics] {
        def fold(semantics: Semantics): semantics.Region = semantics.ref(index)
      }
    }

    // Reification semantics
    trait Reification extends Regions.Reification with Semantics {
      type Language <: Semantics

      def let(value: Region, body: Region): Region = Syntax.Let(value, body)
      def ref(index: Int): Region = Syntax.Ref(index)
    }

    object Reification extends Reification

    // Share-by-value semantics
    trait ByValue extends Semantics with Regions.IgnoreContext {
      type Context = List[base.Region]

      def let(value: Region, body: Region): Region =
        context => body(value(context) :: context)

      def ref(index: Int): Region =
        context => context(index)
    }

    // Share-by-name semantics
    trait ByName extends Semantics with Regions.IgnoreContext {
      type Context = List[Closure]
      case class Closure(context: Context, value: Region)

      def let(value: Region, body: Region): Region =
        context => body(Closure(context, value) :: context)

      def ref(index: Int): Region =
        context1 => context1(index) match {
          case Closure(context2, value) => value(context2)
        }
    }
  }

  // -----------------------
  // HOAS BASED SHARING
  // -----------------------

  object Hoas {
    // Higher-order interface of the sharing extension
    trait Semantics extends Regions.Semantics {
      def let(value: Region)(body: Region => Region): Region
    }

    // How to implement syntax and reification? Hard problem!
    // To fold over a function space, we need the inverse of the semantics :(
    //
    //    object Syntax {
    //      import Regions.Syntax._
    //
    //      case class Let[L <: Semantics](value: Region[L])(body: Region[L] => Region[L]) extends Region[L] {
    //        def fold(semantics: L): semantics.Region = semantics.let(value.fold(semantics)) { argument =>
    //          body(argument.antifold).fold(semantics)
    //      }
    //      }
    //    }
    //
    //    trait Reification extends Regions.Reification with Semantics {
    //      type Language <: Semantics
    //
    //      def let(value: Region)(body: Region => Region): Region = Syntax.Let(value)(body)
    //    }

    // Instead:
    // Mapping the higher-order interface to the de Bruijn interface
    trait ToDeBruijn extends Semantics with Regions.IgnoreContext {
      // The integer is the de Bruijn level of the location of this AST node.
      type Context = Int

      val base: DeBruijn.Semantics

      def let(value: Region)(body: Region => Region): Region =
        outer => base.let(value(outer), body(inner => base.ref(inner - outer - 1))(outer + 1))
    }

    // We can also go the other way around:
    // Mapping the de Bruijn interface to the higher-order interface
    trait FromDeBruijn extends DeBruijn.Semantics with Regions.IgnoreContext {
      type Context = List[base.Region]
      val base: Hoas.Semantics

      def let(value: Region, body: Region): Region =
        context => base.let(value(context)) { argument =>
          body(argument :: context)
        }

      def ref(index: Int): Region =
        context => context(index)
    }
  }

  // Example program. Note that the two bound occurrences of x
  // correspond to different de Bruijn indices.
  def program(semantics: Hoas.Semantics): semantics.Region = {
    import semantics._

    let(univ) { x =>
      let(x) { y =>
        x
      }
    }
  }

  object OurSemantics extends Hoas.ToDeBruijn {
    val base = DeBruijn.Reification
  }

  // Doesn't matter what number we put here.
  println(program(OurSemantics)(15)) 

  // ------------------
  // DERIVED OPERATIONS
  // ------------------

  object Derived {
    // Interface of the derived operation
    trait Semantics extends Regions.Semantics {
      def twice(v: Vector, r: Region): Region
    }

    // Naive implementation of the derived operation
    trait Naive extends Semantics {
      def twice(v: Vector, r: Region): Region =
        union(r, translate(v, r))
    }

    // Sophisticated implementation of the derived operation (with let insertion)
    trait Sophisticated extends Naive with Hoas.Semantics {
      override def twice(v: Vector, r: Region): Region =
        let(r) { cached =>
          super.twice(v, cached)
        }
    }
  }
}
