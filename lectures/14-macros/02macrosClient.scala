import Macros._

object MacrosClient extends App {
  val answer = 42

  println(plus1(answer))
  println(plus1Macro(answer))

  // Let's try out the trace macro!
  trace(42)
  trace(answer)

  trace(expr.m1(m2(m3)))

  // Observe the difference in results.
  trace(plus1Macro(answer))
  trace(plus1(answer))

  //
  traceRaw(plus1Macro(answer))

  val l = List(answer)
  trace(l map (_ + 1) map (_ * 2))

  def f() = l map (_ + 1)
  def g(l2: List[Int]) = l2 map (_ * 2)


  inspectComprehension(someObj.g(f()))


  trace(l flatMap (x => List(x + 1)) map (_ * 2))
  trace(l map (_ + 1) flatMap (x => List(x * 2)))

  trace(inspectComprehension(l map (_ + 1) map (_ * 2)))
  trace(inspectComprehension(l flatMap (x => List(x + 1)) map (_ * 2)))
  trace(inspectComprehension(l map (_ + 1) flatMap (x => List(x * 2))))
}
