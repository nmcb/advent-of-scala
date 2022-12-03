import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim)
      .toList

  var cals: List[List[Int]] = List(List.empty[Int])
  for (line <- input) {
    if   line == "" then cals = cals :+ List.empty[Int]
    else cals = cals.init :+ (cals.last :+ line.toInt)
  }

  val answer1: Int =
    cals.map(_.sum).sorted.last

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    cals.map(_.sum).sorted.takeRight(3).sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
