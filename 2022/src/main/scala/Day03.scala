import scala.io.*

object Day03 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val start1: Long =
    System.currentTimeMillis

  val items: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  def priority(c: Char): Int =
    if c.isLower then c.toInt - 96 else c.toInt - 38

  def priority(line: String): Int =
    assert(line.length % 2 == 0)
    val left  = line.take(line.length / 2)
    val right = line.takeRight(line.length / 2)
    val List(c) = left.toSet.intersect(right.toSet).toList
    priority(c)

  lazy val answer1: Int =
    items.map(priority).sum

  println(s"Answer AOC 2022 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  def priority(lines: List[String]): Int =
    assert(lines.length == 3)
    assert(lines.forall(_.length % 2 == 0))
    val List(r1, r2, r3) = lines
    val List(c) = r1.toSet.intersect(r2.toSet).intersect(r3.toSet).toList
    priority(c)

  lazy val answer2: Int =
    items.grouped(3).map(priority).sum

  println(s"Answer AOC 2022 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
