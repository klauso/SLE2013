import Macros._

object MacrosClient extends App {
  val answer = 42
  trace(42)
  trace(answer)
  trace(plus1Macro(answer))
  trace(plus1(answer))
}
