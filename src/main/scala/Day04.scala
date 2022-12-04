import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val sections: List[(Range,Range)] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map {
        case s"$l1-$r1,$l2-$r2" =>
          (Range.inclusive(l1.toInt,r1.toInt)
          ,Range.inclusive(l2.toInt,r2.toInt)
          )
      }
      .toList

  def contained1(r1: Range, r2: Range): Boolean =
    r1.forall(i => r2.contains(i)) || r2.forall(i => r1.contains(i))

  val answer1: Int =
    sections.count(contained1)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  def contained2(r1: Range, r2: Range): Boolean =
    r1.exists(i => r2.contains(i))

  val answer2: Int =
    sections.count(contained2)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
