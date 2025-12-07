package aoc2025

import nmcb.*

object Day06 extends AoC:

  def calculate(operators: Vector[Char], operands: Vector[Vector[Long]]): Vector[Long] =
    operators.zip(operands).map:
      case ('+', operands) => operands.sum
      case ('*', operands) => operands.product
      case (o, _) => sys.error(s"unknown operator: $o")

  def solve1(lines: Vector[String]): Vector[Long] =
    val operands  = lines.init.map(_.split("\\s+").filter(_.nonEmpty).map(_.toLong)).transpose
    val operators = lines.last.split("\\s+").map(_.head).toVector
    calculate(operators, operands)

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

  lazy val answer1: Long = solve1(lines).sum
  lazy val answer2: Long = solve2(lines).sum
