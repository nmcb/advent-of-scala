import Day15.{Ingredient, day}

object Day17 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  /** Input */

  val containers: List[Int] =
    scala.io.Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toInt)
      .toList

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val fits =
    containers
      .zipWithIndex
      .toSet
      .subsets
      .map(_.toList.map(_._1))
      .filter(_.sum == 150)
      .toList

  lazy val answer1: Int =
      fits.size

  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  lazy val answer2: Int =
    val min = fits.map(_.size).min
    fits.count(_.size == min)

  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
