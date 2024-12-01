import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val values: List[(Int,Int)] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$l   $r" => (l.toInt, r.toInt)
      .toList

  def distance(a: Int, b: Int): Int =
    if a >= b then a - b else b - a

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    val left  = values.map(_._1).sorted
    val right = values.map(_._2).sorted
    left.zip(right).map(distance).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    val left  = values.map(_._1).sorted
    val right = values.map(_._2).sorted
    left.foldLeft(0)((a,l) => a + right.count(_ == l) * l)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

