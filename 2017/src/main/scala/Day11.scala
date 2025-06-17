import scala.io.Source

/** @see https://www.redblobgames.com/grids/hexagons/ */
object Day11 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  enum Dir:
    case N
    case NE
    case SE
    case S
    case SW
    case NW

  import Dir.*

  case class Hex(x: Int, y: Int, z: Int):
    assert(x + y + z == 0)

    infix def move(dir: Dir): Hex =
      dir match
        case N  => Hex(x, y - 1, z + 1)
        case NE => Hex(x + 1, y - 1, z)
        case SE => Hex(x + 1, y, z - 1)
        case S  => Hex(x, y + 1, z - 1)
        case SW => Hex(x - 1, y + 1, z)
        case NW => Hex(x - 1, y, z + 1)

    infix def manhattanDistance(hex: Hex): Int =
      val dx = (x - hex.x).abs
      val dy = (y - hex.y).abs
      val dz = (z - hex.z).abs
      (dx + dy + dz) / 2

  object Hex:
    def zero: Hex =
      Hex(0, 0, 0)

  val path: Vector[Dir] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .map(d => Dir.valueOf(d.toUpperCase))
      .toVector

  val start1: Long = System.currentTimeMillis
  val answer1: Int = path.foldLeft(Hex.zero)(_ move _) manhattanDistance Hex.zero
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = path.scanLeft(Hex.zero)(_ move _).map(_ manhattanDistance Hex.zero).max
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
