import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Box(identifier: String):

    private def idLettersWith(count: Int): Int =
      identifier
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .count((_, occurrences) => occurrences == count)

    val twoCharInId: Boolean =
      idLettersWith(2) >= 1

    val threeCharInId: Boolean =
      idLettersWith(3) >= 1

  object Box:
    val IdCharSet: String = "abcdefghijklmnopqrstuvwxyz"
    def fromString(id: String): Box = Box(id)


  val boxes: List[Box] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Box.fromString)
      .toList

  def solve1(boxes: List[Box]): Int =
    boxes.count(_.twoCharInId) * boxes.count(_.threeCharInId)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(boxes)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def differByOneCharInPlace(id1: String, id2: String): Boolean =
    id1.zip(id2).count(_ != _) == 1

  def solve2(boxes: List[Box]): String =
    val search =
      for
        a <- boxes
        b <- boxes
        if a != b && differByOneCharInPlace(a.identifier, b.identifier)
      yield
        a.identifier intersect b.identifier
    search.headOption.getOrElse(sys.error("unable to find"))

  val start2  = System.currentTimeMillis
  val answer2 = solve2(boxes)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
