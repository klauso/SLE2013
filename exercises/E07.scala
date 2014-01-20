object H07 extends App {
  // Use the material from Lecture 13.

  import Lecture13._

  // ------------------
  // NAME BASED SHARING
  // ------------------

  // The first two tasks are about translating back and forth
  // between names and de Bruijn indices. So we need a variant
  // of the regions language with names.

  object Nominal {
    // Nominal interface of sharing
    trait Semantics extends Regions.Semantics {
      def let(name: String, value: Region)(body: Region): Region
      def ref(name: String): Region
    }

    // Syntax
    object Syntax {
      import Regions.Syntax._

      case class Let[L <: Semantics](name: String, value: Region[L], body: Region[L]) extends Region[L] {
        def fold(semantics: L): semantics.Region = semantics.let(name, value.fold(semantics))(body.fold(semantics))
      }

      case class Ref(name: String) extends Region[Semantics] {
        def fold(semantics: Semantics): semantics.Region = semantics.ref(name)
      }
    }

    // Reification semantics
    trait Reification extends Regions.Reification with Semantics {
      type Language <: Semantics

      def let(name: String, value: Region)(body: Region): Region = Syntax.Let(name, value, body)
      def ref(name: String): Region = Syntax.Ref(name)
    }

    object Reification extends Reification
  }

  // -------------
  // TEST PROGRAMS
  // -------------

  // A test is a program in HOAS and the same program with names.
  // Feel free to add more tests!

  trait Test {
    def hoas(sem: Hoas.Semantics): sem.Region
    def nominal(sem: Nominal.Semantics): sem.Region
  }

  object Test1 extends Test {
    def hoas(sem: Hoas.Semantics): sem.Region = {
      import sem._
      let(univ) { x =>
        let(x) { y =>
          x
        }
      }
    }

    def nominal(sem: Nominal.Semantics): sem.Region = {
      import sem._
      let("x1", univ) {
        let("x2", ref("x1")) {
          ref("x1")
        }
      }
    }
  }

  object Test2 extends Test {
    def hoas(sem: Hoas.Semantics): sem.Region = {
      import sem._
      let(univ) { x =>
        let(x) { y =>
          y
        }
      }
    }

    def nominal(sem: Nominal.Semantics): sem.Region = {
      import sem._
      let("x1", univ) {
        let("x2", ref("x1")) {
          ref("x2")
        }
      }
    }
  }

  val tests = List(Test1, Test2)

  // -------------------------------------------------
  // TASK 1: Translate from de Bruijn indices to names
  // -------------------------------------------------

  trait DeBruijnToNominal extends Regions.IgnoreContext with DeBruijn.Semantics {
    // Generating fresh variables
    var number = 0
    def fresh(): String = {
      number += 1
      "x" + number
    }

    // We use a list of names as context
    type Context = List[String]

    // We transform into programs in the nominal language
    val base: Nominal.Semantics

    // Fill in the operations!
    def let(value: Region, body: Region): Region = ???
    def ref(index: Int): Region = ???
  }

  // To test the DeBruijnToNominal semantics,
  // we first convert from the HOAS program to de Bruijn,
  // and then from de Bruijn to Nominal,
  // and then we reify both programs to a nominal AST and compare them.
  for (test <- tests) {
    val expected = test.nominal(Nominal.Reification)

    val actual = test.hoas(new Hoas.ToDeBruijn {
      val base = new DeBruijnToNominal {
        val base = Nominal.Reification
      }
    })(0)(List())

    assert(expected == actual, s"expected ${expected} but found ${actual}")
  }

  // -------------------------------------------------
  // TASK 2: Translate from names to de Bruijn indices
  // -------------------------------------------------

  trait NominalToDeBruijn extends Regions.IgnoreContext with Nominal.Semantics {
    // We use a list of names as context
    type Context = List[String]

    // We transform into programs in the de Bruijn language
    val base: DeBruijn.Semantics

    // Fill in the operations!
    def let(name: String, value: Region)(body: Region): Region = ???
    def ref(name: String): Region = ???
  }

  // To test the NominalToDeBruijn semantics,
  // we convert one program from Nominal to de Bruijn,
  // and the other from Hoas to de Bruijn,
  // and then reify both programs to a de Bruijn AST and compare them.
  for (test <- tests) {
    val expected = test.hoas(new Hoas.ToDeBruijn {
      val base = DeBruijn.Reification
    })(0)

    val actual = test.nominal(new NominalToDeBruijn {
      val base = DeBruijn.Reification
    })(List())

    assert(expected == actual, s"expected ${expected} but found ${actual}")
  }

  // ------------------------------------------------------------
  // TASK 3: Measure code duplication and computation duplication
  // ------------------------------------------------------------

  // A semantics that measures AST size
  // (Fill in the operations!)
  trait MeasureSize extends Regions.Semantics {
    type Region = Int

    def univ: Region = 1
    def empty: Region = ???
    def circle: Region = ???
    def scale(v: Vector, r: Region): Region = 1 + r
    def union(r: Region, s: Region): Region = ???
    def translate(v: Vector, r: Region): Region = ???
  }
  
  // A base trait for semantics that measures computation steps
  trait MeasureStepsBase extends Regions.Semantics {
    val base: Regions.PointInRegion

    type Region = base.Region

    def stepping(r: Region): Region

    override def univ: Region = stepping(base.univ)
    override def empty: Region = stepping(base.empty)
    override def circle: Region = stepping(base.circle)
    override def scale(v: Vector, r: Region): Region = stepping(base.scale(v, r))
    override def union(r: Region, s: Region): Region = stepping(base.union(r, s))
    override def translate(v: Vector, r: Region): Region = stepping(base.translate(v, r))
  }

  // First semantics that measures computation steps
  // (by adding side effects to the point-in-region semantics)
  trait MeasureSteps1 extends MeasureStepsBase {
    var steps = 0
    def stepping(r: Region): Region = (x, y) => {
      steps += 1
      r(x, y)
    }
  }

  // Second semantics that measures computation steps
  // (by adding side effects in a different way)
  trait MeasureSteps2 extends MeasureStepsBase {
    var steps = 0
    def stepping(r: Region): Region = {
      steps += 1
      (x, y) => r(x, y)
    }
  }

  // Use the measurement semantics MeasureSize, MeasureSteps1
  // and/or MeasureSteps2 to measure the difference between:
  //
  // 1. DeBruijn.ByValue and DeBruijn.ByName
  // 2. Derived.Naive and Derived.Sophisticated
  //
  // Sometimes, the different measurement semantics give different
  // results. Explain the differences!

  // -------------
  // OPTIONAL TASK
  // -------------

  // Implement inlining of let-expressions where the
  // variable is used at most once.
  //
  // For example, when using the semantics,
  //
  //   let(union(univ, empty)) {x =>
  //     let(scale((1, 2), x)) {y =>
  //       union(y, y)
  //     }
  //   }
  //
  // should behave like:
  //
  //  let(scale((1, 2), union(univ, empty))) {y =>
  //    union(y, y)
  //
  // The first let gets inlined away, because x is only used once.
  // But the second let stays, because y is used more than once.
}
