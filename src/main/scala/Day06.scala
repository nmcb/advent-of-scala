import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  enum Dir:
    case N, E, S, W

  import Dir.*

  case class Pos(x: Int, y: Int, d: Dir):
    def step: Pos =
      d match
        case N => copy(y = y - 1)
        case S => copy(y = y + 1)
        case E => copy(x = x + 1)
        case W => copy(x = x - 1)

    def right: Pos =
      d match
        case N => copy(d = E)
        case S => copy(d = W)
        case E => copy(d = S)
        case W => copy(d = N)


  case class Grid(positions: List[List[Char]]):
    val sizeX: Int = positions.head.size
    val sizeY: Int = positions.size

    def peek(x: Int, y: Int): Char =
      if x >= 0 & y >= 0 & x < sizeX & y < sizeY then positions(y)(x) else ' '

    def peek(p: Pos): Char =
      peek(p.x, p.y)

    val start: Pos =
      val guards =
        for {
          x <- 0 until sizeX
          y <- 0 until sizeY
          if peek(x, y) == '^'
        } yield Pos(x, y, N)
      guards.head

    def walk1(pos: Pos, result: Vector[Pos] = Vector.empty): Vector[Pos] =
      peek(pos.step) match
        case ' '       => result :+ pos
        case '.' | '^' => walk1(pos.step, result :+ pos)
        case '#'       => walk1(pos.right, result)

    def solve1: Int =
      walk1(start).map(p => (p.x, p.y)).distinct.size

    def peek2(p: Pos, overruleX: Int, overruleY: Int): Char =
      if p.x == overruleX & p.y == overruleY then '#' else peek(p.x, p.y)

    def walkCircular(pos: Pos, overruleX: Int, overruleY: Int, result: Vector[Pos] = Vector.empty): Boolean =
      if result.contains(pos) then
        true
      else
        peek2(pos.step, overruleX, overruleY) match
          case ' '       => false
          case '.' | '^' => walkCircular(pos.step, overruleX, overruleY, result :+ pos)
          case '#'       => walkCircular(pos.right, overruleX, overruleY, result)

    val empties: List[(Int,Int)] =
      for {
        (p, y) <- positions.zipWithIndex
        (c, x) <- p.zipWithIndex
        if c == '.'
      } yield (x, y)

    def solve2: Int =
      empties.count: (x,y) =>
        walkCircular(start, x, y)



  private val grid: Grid =
    Grid(Source.fromResource(s"input$day.txt").getLines.map(_.toList).toList)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = grid.solve1
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = grid.solve2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
