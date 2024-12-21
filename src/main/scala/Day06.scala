import scala.io.*
import nmcb.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  import Dir.*

  private val grid: Grid[Char] = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)

  extension (g: Grid[Char])

    def walkGuard(pos: Pos, dir: Dir, result: Set[Pos] = Set.empty): Set[Pos] =
      val next = pos + dir
      g.peekOrElse(next, ' ') match
        case ' '       => result + pos
        case '.' | '^' => walkGuard(next, dir, result + pos)
        case '#'       => walkGuard(pos, dir.ccw, result)

    def peekWithObstruction(p: Pos, obstruct: Pos): Char =
        if p == obstruct then '#' else g.peekOrElse(p, ' ')

    def walkCircular(pos: Pos, dir: Dir, obstruct: Pos, visited: Set[(Pos,Dir)] = Set.empty): Boolean =
      if visited.contains((pos, dir)) then
        true
      else
        val next = pos + dir
        g.peekWithObstruction(next, obstruct) match
          case ' '       => false
          case '.' | '^' => walkCircular(next, dir, obstruct, visited + ((pos, dir)))
          case '#'       => walkCircular(pos, dir.cw, obstruct, visited)


  val start: Pos   = grid.findOne('^')
  val time1: Long = System.currentTimeMillis
  val answer1: Int = grid.walkGuard(start, N).size
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - time1}ms]")


  val obstructions: Set[Pos] = grid.walkGuard(start, N) - start
  val time2: Long  = System.currentTimeMillis
  val answer2: Int = obstructions.count(o => grid.walkCircular(start, N, o))
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - time2}ms]")
