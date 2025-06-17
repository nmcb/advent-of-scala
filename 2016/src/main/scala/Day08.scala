import scala.io.*

object Day08 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Operation:
    case Rectangle(sizeX: Int, sizeY: Int)
    case RotateCol(index: Int, by: Int)
    case RotateRow(index: Int, by: Int)

  object Operation:
    def fromString(s: String): Operation =
      s match
        case s"rect ${sizeX}x${sizeY}"        => Rectangle(sizeX = sizeX.toInt, sizeY = sizeY.toInt)
        case s"rotate column x=$index by $by" => RotateCol(index = index.toInt, by = by.toInt)
        case s"rotate row y=$index by $by"    => RotateRow(index = index.toInt, by = by.toInt)

  val operations: Vector[Operation] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Operation.fromString)
      .toVector

  object Screen:

    val empty: Screen =
      Screen(List.fill(6, 50)('.'))


  case class Screen(pixels: List[List[Char]]):
    import Operation.*

    val sizeX: Int = pixels.head.size
    val sizeY: Int = pixels.size

    def countLit: Int =
      pixels.map(_.count(_ == '#')).sum

    def asString: String =
      pixels.map(_.mkString).mkString("\n")

    def transpose: Screen =
      Screen(pixels.transpose)

    def rectangle(sizeX: Int, sizeY: Int): Screen =
      val (top, bottom) = pixels.splitAt(sizeY)
      val updated = top.map(row => List.fill(sizeX)('#') :++ row.drop(sizeX))
      Screen(updated :++ bottom)

    def shiftRow(index: Int, by: Int): Screen =
      val (top, bottom) = pixels.splitAt(index)
      val (row, rest)   = bottom.splitAt(1)
      val shifted       = Iterator.continually(row.head).flatten.drop(sizeX - by).take(sizeX).toList
      Screen(top :+ shifted :++ rest)

    infix def process(op: Operation): Screen =
      op match
        case Rectangle(sizeX, sizeY) => rectangle(sizeX, sizeY)
        case RotateRow(index, by)    => shiftRow(index, by)
        case RotateCol(index, by)    => transpose.shiftRow(index, by).transpose

  val start1: Long = System.currentTimeMillis
  val answer1: Int = operations.foldLeft(Screen.empty)(_ process _).countLit
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long    = System.currentTimeMillis
  val answer2: String = operations.foldLeft(Screen.empty)(_ process _).asString
  println(s"Answer day $day part 2:\n$answer2 [${System.currentTimeMillis - start2}ms]")
