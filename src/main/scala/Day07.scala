import nmcb.*
import predef.*

import scala.io.*

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
      Equation
        .combinations1(arguments.length - 1, operators)
        .exists: combination =>
          val (computation: Long, _) = combination.foldLeft((arguments.head, arguments.tail)):
            case ((accumulator, remaining), operator) => (operator(accumulator)(remaining.head) , remaining.tail)
          computation == result

  val input: List[Equation] =
    Source.fromResource(s"input$day.txt").getLines
      .map:
        case s"$result: $arguments" =>
          Equation(result.toLong, arguments.split(' ').map(_.toLong).toList)
      .toList

  object Equation:

    val cache = memo[(Int, Operators), Combinations]()
    def combinations1(n: Int, operators: Operators): Combinations =
      cache.memoize(n, operators):
        def leftPad(todo: Operators, padTo: Combinations, result: Combinations = List.empty): Combinations =
          todo match
            case Nil => result
            case h :: t => leftPad(t, padTo, result ++ padTo.map(h +: _))

        def loop(todo: Int, result: Combinations = List(List.empty)): Combinations =
          if todo <= 0 then result else loop(todo - 1, leftPad(operators, result))

        loop(n)

    def combinations2[A](n: Int, elements: List[A]): Iterator[List[A]] =
      val m = elements.length
      val size = math.pow(m, n).toInt
      val divs = List.unfold(1)(i => Option.when(i * m <= size)(i, i * m)).reverse
      def generate(row: Int)(idx: Int): A = elements(row / divs(idx) % m)
      def combination(row: Int): List[A] = List.tabulate(n)(generate(row))
      Iterator.tabulate(size)(combination)

    def combinations3[A](n: Int, elements: List[A]): Iterator[List[A]] =
      if n > 0 then
        for {
          h <- elements.iterator
          t <- combinations3(n - 1, elements)
        } yield h :: t
      else
        Iterator.single(List.empty)


  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.filter(_.valid(OperatorsPart1)).map(_.result).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = input.filter(_.valid(OperatorsPart2)).map(_.result).sum
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


  /** https://github.com/stewSquared/advent-of-code/blob/master/src/main/scala/2024/Day07.worksheet.sc */
  def solvable(equation: Equation): Boolean =
    def loop(lhs: Long, rhs: List[Long]): Boolean =
      rhs match
        case Nil => ???
        case head :: Nil => head == lhs
        case n :: ns =>
          /* complement addition       */
          loop(lhs - n, ns)
            /* complement multiplication */
            || ((lhs % n == 0) && loop(lhs / n, ns))
            /* complement concatenation  */
            || (lhs > n && lhs.toString.endsWith(n.toString) && loop(lhs.toString.dropRight(n.toString.length).toLong, ns))

    loop(equation.result, equation.arguments.reverse)

  val start2Stewart: Long  = System.currentTimeMillis
  val answer2Stewart: Long = input.filter(solvable).map(_.result).sum
  println(s"Day $day answer part 2: $answer2Stewart [${System.currentTimeMillis - start2Stewart}ms] (Stewart)")
