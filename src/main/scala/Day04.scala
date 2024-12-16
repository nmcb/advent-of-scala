import nmcb.*
import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  private type Move = Pos => Pos

  val N:  Move = p => Pos(p.x, p.y - 1)
  val E:  Move = p => Pos(p.x + 1, p.y)
  val S:  Move = p => Pos(p.x, p.y + 1)
  val W:  Move = p => Pos(p.x - 1, p.y)
  val NE: Move = p => Pos(p.x + 1, p.y - 1)
  val SE: Move = p => Pos(p.x + 1, p.y + 1)
  val NW: Move = p => Pos(p.x - 1, p.y - 1)
  val SW: Move = p => Pos(p.x - 1, p.y + 1)

  val grid: Grid[Char] = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)
  extension (g: Grid[Char])

    def read(n: Int)(next: Move, p: Pos, result: String = ""): String =
      if result.length == n then result else g.read(n)(next, next(p), result :+ g.peekOrElse(p, '.'))

    def startsXMAS(p: Pos): Int =
      List(N, E, S, W, NE, SE, NW, SW).foldLeft(0): (c, n) =>
        if read(4)(n, p) == "XMAS" then c + 1 else c

    def hasXMAS(p: Pos): Boolean =
      val l1 = g.read(3)(SE, NW(p))
      val l2 = g.read(3)(NE, SW(p))
      (l1 == "MAS" | l1 == "SAM") && (l2 == "MAS" | l2 == "SAM")


  val start1: Long = System.currentTimeMillis
  val answer1: Int = grid.positions.foldLeft(0)((c, p) => c + grid.startsXMAS(p))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val start2: Long = System.currentTimeMillis
  val answer2: Int = grid.positions.count(hasXMAS(grid))
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
