import scala.io.*

object Day06 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val lines = Source.fromResource(s"input$day.txt").getLines.toVector

  def calculate(operators: Vector[Char], operands: Vector[Vector[Long]]): Vector[Long] =
    operators.zip(operands).map:
      case ('+', operands) => operands.sum
      case ('*', operands) => operands.product
      case (o, _) => sys.error(s"unknown operator: $o")

  def solve1(lines: Vector[String]): Vector[Long] =
    val operands  = lines.init.map(_.split("\\s+").filter(_.nonEmpty).map(_.toLong)).transpose
    val operators = lines.last.split("\\s+").map(_.head).toVector
    calculate(operators, operands)


  val start1  = System.currentTimeMillis
  def answer1 = solve1(lines).sum
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  extension (column: Vector[Char])

    def isSeparator: Boolean =
      column.forall(_ == ' ')

    def toLong: Long =
      column.filter(_.isDigit).mkString("").toLong

  def solve2(lines: Vector[String]): Vector[Long] =

    val operands = lines.init.transpose.foldRight(Vector(Vector.empty[Long])):
      case (column, result) if column.isSeparator => result :+ Vector.empty[Long]
      case (column, result :+ current)            => result :+ (current :+ column.toLong)
      case (column, _)                            => sys.error(s"unparsable: $column")

    val operators = lines.last.split("\\s+").map(_.head).toVector.reverse

    calculate(operators, operands)

  val start2  = System.currentTimeMillis
  def answer2 = solve2(lines).sum
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
