import scala.io.*
import collection.mutable

object Day07 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Operator     = Long => Long => Long
  type Operators    = List[Operator]
  type Combinations = List[Operators]

  val addition: Operator       = (l: Long) => (r: Long) => l + r
  val multiplication: Operator = (l: Long) => (r: Long) => l * r
  val concatenation: Operator  = (l: Long) => (r: Long) => (l.toString + r.toString).toLong

  val OperatorsPart1 = List(addition, multiplication)
  val OperatorsPart2 = List(addition, multiplication, concatenation)

  case class Equation(result: Long, arguments: List[Long]):

    def valid(operators: Operators): Boolean =
      Equation.combinations(arguments.length - 1, operators)
        .exists: list =>
          val (computation: Long, _) =
            list.foldLeft((arguments.head, arguments.tail)):
              case ((acc, arg), operator) => (operator(acc)(arg.head) , arg.tail)
          computation == result

  val input: List[Equation] =
    Source.fromResource(s"input$day.txt").getLines
      .map:
        case s"$result: $arguments" =>
          Equation(result.toLong, arguments.split(' ').map(_.toLong).toList)
      .toList

  object Equation:
    val cache: mutable.Map[(Int, Operators), Combinations] = mutable.Map.empty
    def combinations(n: Int, operators: Operators): Combinations = cache.getOrElseUpdate((n, operators), {
      def leftPad(todo: Operators, padTo: Combinations, result: Combinations = List.empty): Combinations =
        todo match
          case Nil => result
          case h :: t => leftPad(t, padTo, result ++ padTo.map(h :: _))

      def loop(todo: Int, result: Combinations = List(List.empty)): Combinations =
        if todo == 0 then result else loop(todo - 1, leftPad(operators, result))

      loop(n)
    })

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.filter(_.valid(OperatorsPart1)).map(_.result).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = input.filter(_.valid(OperatorsPart2)).map(_.result).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
