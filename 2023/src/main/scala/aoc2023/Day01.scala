package aoc2023

import nmcb.*

object Day01 extends AoC:

  val digits: Map[String, Char] =
    (0 to 9).map(_.toString).map(n => n -> n.head).toMap

  def recover(replacements: Map[String, Char])(line: String): Int =

    def firstLeftDigit(side: String): (String, Char) =
      replacements
        .find((p,_) => side.startsWith(p))
        .getOrElse(firstLeftDigit(side.tail))


    def firstRightDigit(side: String): (String, Char) =
      replacements
        .find((p,_) => side.endsWith(p))
        .getOrElse(firstRightDigit(side.init))

    val (_,l) = firstLeftDigit(line)
    val (_,r) = firstRightDigit(line)
    s"$l$r".toInt

  val names: Map[String, Char] =
    Map(
      "one"   -> '1',
      "two"   -> '2',
      "three" -> '3',
      "four"  -> '4',
      "five"  -> '5',
      "six"   -> '6',
      "seven" -> '7',
      "eight" -> '8',
      "nine"  -> '9',
      "zero"  -> '0'
    )

  lazy val answer1: Int = lines.map(recover(digits)).sum
  lazy val answer2: Int = lines.map(recover(digits ++ names)).sum
