package aoc2020

import nmcb.*

object Day06 extends AoC:


  val answers: Vector[Vector[String]] =
    lines.foldLeft(Vector(Vector.empty[String])):
      case (result, line) if line.nonEmpty => (line +: result.head) +: result.tail
      case (result, _)                     => Vector.empty[String] +: result

  def chars(list: Vector[String]): String =
    list.fold("")(_+_).distinct
    
  lazy val answer1: Int = answers.map(chars).map(_.length).sum
  lazy val answer2: Int = answers.map(grp => grp.fold(chars(grp))(_ intersect _).length).sum
