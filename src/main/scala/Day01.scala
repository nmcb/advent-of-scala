import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val (left, right): (List[Int], List[Int]) =
    val values =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map:
          case s"$l   $r" => (l.toInt, r.toInt)
        .toList
    (values.map(_._1).sorted, values.map(_._2).sorted)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = left.zip(right).map((l,r) => if l >= r then l - r else r - l).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = left.foldLeft(0)((a,l) => a + right.count(_ == l) * l)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
