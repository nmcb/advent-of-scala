package aoc2024

import nmcb.*
import nmcb.predef.*

import scala.annotation.tailrec

object  Day07 extends AoC:

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
            case ((accumulator, remaining), operator) => (operator(accumulator)(remaining.head), remaining.tail)
          computation == result

  val equations: Vector[Equation] = lines.map:
    case s"$result: $arguments" => Equation(result.toLong, arguments.split(' ').map(_.toLong).toList)

  object Equation:

    private val cache = memo[(Int, Operators), Combinations]()
    def combinations1(n: Int, operators: Operators): Combinations = cache.memoize(n, operators):

      @tailrec
      def leftPad(todo: Operators, padTo: Combinations, result: Combinations = List.empty): Combinations =
        todo match
          case Nil => result
          case h :: t => leftPad(t, padTo, result ++ padTo.map(h +: _))

      @tailrec
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


  lazy val answer1: Long = equations.filter(_.valid(OperatorsPart1)).map(_.result).sum
  lazy val answer2: Long = equations.filter(_.valid(OperatorsPart2)).map(_.result).sum


  /** https://github.com/stewSquared/advent-of-code/blob/master/src/main/scala/2024/Day07.worksheet.sc */
  def solvable(equation: Equation): Boolean =

    def loop(lhs: Long, rhs: List[Long]): Boolean =
      rhs match
        case Nil =>
          sys.error(s"empty right hand side")
        case head :: Nil =>
          head == lhs
        case n :: ns =>
          /* complement addition       */
          loop(lhs - n, ns)
            /* complement multiplication */
            || ((lhs % n == 0) && loop(lhs / n, ns))
            /* complement concatenation  */
            || (lhs > n && lhs.toString.endsWith(n.toString) && loop(lhs.toString.dropRight(n.toString.length).toLong, ns))

    loop(equation.result, equation.arguments.reverse )

  lazy val answer2Stewart: Long = equations.filter(solvable).map(_.result).sum
