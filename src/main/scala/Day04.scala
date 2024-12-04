import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):
    def move(move: Move): Pos = move(this)

  private type Move = Pos => Pos

  val N: Move = p => Pos(p.x, p.y - 1)
  val E: Move = p => Pos(p.x + 1, p.y)
  val S: Move = p => Pos(p.x, p.y + 1)
  val W: Move = p => Pos(p.x - 1, p.y)
  val NE: Move = p => Pos(p.x + 1, p.y - 1)
  val SE: Move = p => Pos(p.x + 1, p.y + 1)
  val NW: Move = p => Pos(p.x - 1, p.y - 1)
  val SW: Move = p => Pos(p.x - 1, p.y + 1)

  case class Grid(lines: Array[Array[Char]]):
    private val maxX: Int = lines.map(_.length).max
    private val maxY: Int = lines.length

    private val positions: Seq[Pos] =
      for {
        x <- 0 until maxX
        y <- 0 until maxY
      } yield Pos(x, y)

    private def peek(p: Pos): Char =
      if p.x < 0 | p.x >= maxX | p.y < 0 | p.y >= maxY then '.' else lines(p.y)(p.x)

    private def read(n: Int)(next: Move, p: Pos, result: String = ""): String =
      if result.length == n then
        result
      else
        read(n)(next, next(p), result :+ peek(p))

    private def startsXMAS(p: Pos): Int =
      List(N, E, S, W, NE, SE, NW, SW)
        .foldLeft(0)((c,n) => if read(4)(n, p) == "XMAS" then c + 1 else c)

    def solve1: Int =
      positions.foldLeft(0)((c,p) => c + startsXMAS(p))

    private def hasXMAS(p: Pos): Boolean =
      val l1 = read(3)(SE, p.move(NW))
      val l2 = read(3)(NE, p.move(SW))
      (l1 == "MAS" | l1 == "SAM") && (l2 == "MAS" | l2 == "SAM")

    def solve2: Int =
      positions.count(hasXMAS)

  private val grid: Grid =
    Grid(Source.fromResource(s"input$day.txt").getLines.map(_.toArray).toArray)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = grid.solve1
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = grid.solve2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
