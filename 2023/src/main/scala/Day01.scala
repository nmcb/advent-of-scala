import scala.io.*

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val values: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

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

  val start1: Long =
    System.currentTimeMillis

  lazy val answer1: Int =
    values.map(recover(digits)).sum

  println(s"Answer AOC 2023 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

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

  lazy val answer2: Int =
    values.map(recover(digits ++ names)).sum

  println(s"Answer AOC 2023 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
