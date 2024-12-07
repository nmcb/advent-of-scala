import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  enum Dir:
    case N, E, S, W
    def right: Dir =
      this match
        case N => E
        case S => W
        case E => S
        case W => N

  import Dir.*

  case class Pos(x: Int, y: Int):
    def step(dir: Dir): Pos =
      dir match
        case N => copy(y = y - 1)
        case S => copy(y = y + 1)
        case E => copy(x = x + 1)
        case W => copy(x = x - 1)


  case class Grid(positions: Vector[Vector[Char]]):
    val sizeX: Int = positions.head.size
    val sizeY: Int = positions.size

    def peek(p: Pos): Char =
      if p.x >= 0 & p.y >= 0 & p.x < sizeX & p.y < sizeY then positions(p.y)(p.x) else ' '

    val start: Pos =
      val guards =
        for {
          x <- 0 until sizeX
          y <- 0 until sizeY
          if positions(y)(x) == '^'
        } yield Pos(x, y)
      guards.head

    def walkGuard(pos: Pos, dir: Dir, result: Set[Pos] = Set.empty): Set[Pos] =
      val next = pos.step(dir)
      peek(next) match
        case ' '       =>
          result + pos
        case '.' | '^' =>
          walkGuard(next, dir, result + pos)
        case '#'       =>
          walkGuard(pos, dir.right, result)

    def solve1: Int =
      walkGuard(start, N).size

    def peekWithObstruction(p: Pos, obstruct: Pos): Char =
      if p == obstruct then '#' else peek(p)

    val possibleObstructions: Set[Pos] =
      walkGuard(start, N) - start

    def walkCircular(pos: Pos, dir: Dir, obstruct: Pos, visited: Set[(Pos, Dir)] = Set.empty): Boolean =
      if visited.contains((pos, dir)) then
        true
      else
        val next = pos.step(dir)
        peekWithObstruction(next, obstruct) match
          case ' ' =>
            false
          case '.' | '^' =>
            walkCircular(next, dir, obstruct, visited + ((pos, dir)))
          case '#' =>
            walkCircular(pos, dir.right, obstruct, visited)

    def solve2: Int =
      possibleObstructions.count(p => walkCircular(start, N, p))


  private val grid: Grid =
    Grid(Source.fromResource(s"input$day.txt").getLines.map(_.toVector).toVector)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = grid.solve1
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = grid.solve2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
