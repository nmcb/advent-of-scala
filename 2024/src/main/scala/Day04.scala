import nmcb.*
import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  private type Direction = Pos => Pos

  val N:  Direction = p => Pos(p.x, p.y - 1)
  val E:  Direction = p => Pos(p.x + 1, p.y)
  val S:  Direction = p => Pos(p.x, p.y + 1)
  val W:  Direction = p => Pos(p.x - 1, p.y)
  val NE: Direction = p => Pos(p.x + 1, p.y - 1)
  val SE: Direction = p => Pos(p.x + 1, p.y + 1)
  val NW: Direction = p => Pos(p.x - 1, p.y - 1)
  val SW: Direction = p => Pos(p.x - 1, p.y + 1)

  val grid: Grid[Char] = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)
  extension (grid: Grid[Char])

    def read(length: Int, direction: Direction, from: Pos, result: String = ""): String =
      if length == 0 then
        result
      else
        grid.read(length - 1, direction, direction(from), result :+ grid.peekOrElse(from, '.'))

    def startsXMAS(position: Pos): Int =
      List(N, E, S, W, NE, SE, NW, SW).foldLeft(0): (count, direction) =>
        if read(4, direction, position) == "XMAS" then count + 1 else count

    def hasXMAS(position: Pos): Boolean =
      val line1 = grid.read(3, direction = SE, from = NW(position))
      val line2 = grid.read(3, direction = NE, from = SW(position))
      (line1 == "MAS" | line1 == "SAM") && (line2 == "MAS" | line2 == "SAM")


  val start1: Long = System.currentTimeMillis
  val answer1: Int = grid.positions.foldLeft(0)((count, position) => count + grid.startsXMAS(position))
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val start2: Long = System.currentTimeMillis
  val answer2: Int = grid.positions.count(hasXMAS(grid))
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
