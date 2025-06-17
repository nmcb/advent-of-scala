import scala.io.*

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val (left, right): (Seq[Int], Seq[Int]) =
    val values =
      Source.fromResource(s"input$day.txt").getLines.toSeq.map:
        case s"$l   $r" => (l.toInt, r.toInt)

    (values.map(_._1).sorted, values.map(_._2).sorted)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = left.zip(right).map(_ - _).map(math.abs).sum
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = left.foldLeft(0)((a,l) => a + l * right.count(_ == l))
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
