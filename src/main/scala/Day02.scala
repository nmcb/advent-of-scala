import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Line(instructions: Vector[Char])

  object Line:
    def fromString(s: String): Line =
      Line(s.toVector)

  val input: Vector[Line] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Line.fromString)
      .toVector

  case class Pad(pad: Vector[String], x: Int, y: Int):

    val current: Char =
      pad(y)(x)

    infix def advance(i: Char): Pad =
      val (nx, ny) =
        i match
          case 'U' => (x, y - 1)
          case 'D' => (x, y + 1)
          case 'L' => (x - 1, y)
          case 'R' => (x + 1, y)
      if pad(ny)(nx) != ' ' then copy(x = nx, y = ny) else this

    infix def process(l: Line): Pad =
      l.instructions.foldLeft(this)(_ advance _)

  object Pad:

    def pad1: Pad =
      Pad(
        pad =
          Vector(
            "     ",
            " 123 ",
            " 456 ",
            " 789 ",
            "     "
          ),
        x = 2,
        y = 2)

    def pad2: Pad =
      Pad(
        pad =
          Vector(
            "       ",
            "   1   ",
            "  234  ",
            " 56789 ",
            "  ABC  ",
            "   D   ",
            "       "
          ),
        x = 1,
        y = 3)

  def solve(pad: Pad, lines: Vector[Line]): String =
    lines.scanLeft(pad)(_ process _).map(_.current).tail.mkString

  val start1: Long    = System.currentTimeMillis
  val answer1: String = solve(Pad.pad1, input)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long    = System.currentTimeMillis
  val answer2: String = solve(Pad.pad2, input)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
