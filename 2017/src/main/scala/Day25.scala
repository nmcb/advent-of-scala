import scala.io.Source

object Day25 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  val lines: Vector[String] =
    Source.fromResource(s"input$day.txt").getLines.toVector

  enum Move:
    case L, R

  import Move.*

  type State       = String
  type Value       = Int
  type Position    = Int
  type Tape        = Map[Position,Value]
  type Transitions = Map[(State,Value),(State,Value,Move)]

  /**
   * Begin in state A.
   * Perform a diagnostic checksum after 12317297 steps.
   *
   * In state A:
   *   If the current value is 0:
   *     - Write the value 1.
   *     - Move one slot to the right.
   *     - Continue with state B.
   *   If the current value is 1:
   *     - Write the value 0.
   *     - Move one slot to the left.
   *     - Continue with state D.
   *
   * In state B:
   *   If the current value is 0:
   *     - Write the value 1.
   *     - Move one slot to the right.
   *     - Continue with state C.
   *   If the current value is 1:
   *     - Write the value 0.
   *     - Move one slot to the right.
   *     - Continue with state F.
   *
   * In state C:
   *   If the current value is 0:
   *     - Write the value 1.
   *     - Move one slot to the left.
   *     - Continue with state C.
   *   If the current value is 1:
   *     - Write the value 1.
   *     - Move one slot to the left.
   *     - Continue with state A.
   *
   * In state D:
   *   If the current value is 0:
   *     - Write the value 0.
   *     - Move one slot to the left.
   *     - Continue with state E.
   *   If the current value is 1:
   *     - Write the value 1.
   *     - Move one slot to the right.
   *     - Continue with state A.
   *
   * In state E:
   *   If the current value is 0:
   *     - Write the value 1.
   *     - Move one slot to the left.
   *     - Continue with state A.
   *   If the current value is 1:
   *     - Write the value 0.
   *     - Move one slot to the right.
   *     - Continue with state B.
   *
   * In state F:
   *   If the current value is 0:
   *     - Write the value 0.
   *     - Move one slot to the right.
   *     - Continue with state C.
   *   If the current value is 1:
   *     - Write the value 0.
   *     - Move one slot to the right.
   *     - Continue with state E.
   *
   */
  val beginState: State = "A"
  val steps: Int = 12317297
  val transitions: Transitions =
    Map(
      ("A", 0) -> ("B", 1, R),
      ("A", 1) -> ("D", 0, L),
      ("B", 0) -> ("C", 1, R),
      ("B", 1) -> ("F", 0, R),
      ("C", 0) -> ("C", 1, L),
      ("C", 1) -> ("A", 1, L),
      ("D", 0) -> ("E", 0, L),
      ("D", 1) -> ("A", 1, R),
      ("E", 0) -> ("A", 1, L),
      ("E", 1) -> ("B", 0, R),
      ("F", 0) -> ("C", 0, R),
      ("F", 1) -> ("E", 0, R),
    )

  case class Turing(
    transitions: Transitions,
    state: State,
    cursor: Position = 0,
    tape: Tape = Map.empty.withDefaultValue(0)
  ):

    def step: Turing =

      val (nstate, nvalue, move) =
        transitions((state, tape(cursor)))

      val ncursor =
        move match
          case L => cursor - 1
          case R => cursor + 1

      val ntape =
        tape + (cursor -> nvalue)

      copy(state = nstate, cursor = ncursor, tape = ntape)

  def turing: Turing = Turing(transitions, beginState)

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = Iterator.iterate(turing)(_.step).drop(steps).next.tape.count((s,v) => v == 1)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
