import scala.annotation.*
import scala.io.*

object Day15 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Dir = Char

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Pos =
      d match
        case '^' => copy(y = y - 1)
        case '>' => copy(x = x + 1)
        case 'v' => copy(y = y + 1)
        case '<' => copy(x = x - 1)

    def coordinate: Int =
      100 * y + x


  case class Grid(grid: Map[Pos,Char]):
    val sizeX: Int = grid.keys.maxBy(_.x).x + 1
    val sizeY: Int = grid.keys.maxBy(_.y).y + 1
    val boxes: Set[Pos] = grid.filter((_,c) => c == 'O' | c == '[').keySet
    val robot: Pos      = grid.find((_,c) => c == '@').map(_._1).head

    override def toString: String =
      val representation = for {
        y <- 0 until sizeY
        x <- 0 until sizeX
        p = Pos(x, y)
      } yield grid(p)
      representation.mkString("").grouped(sizeX).mkString("\n")

    def isFree(p: Pos): Boolean = grid(p) == '.'
    def isWall(p: Pos): Boolean = grid(p) == '#'
    def isBox(p: Pos): Boolean  = grid(p) == 'O' | grid(p) == ']' | grid(p) == '['

    def moves(p: Pos, d: Dir): Set[(Pos,Pos)] =
      def loop(tos: Set[Pos], updates: Set[(Pos,Pos)] = Set.empty): Set[(Pos,Pos)] =

        def widen(c: Set[Pos]): Set[Pos] =
          val pl = c.minBy(_.x)
          val pr = c.maxBy(_.x)
          val l = if (d == '^' | d == 'v') & grid(pl) == ']' then Set(pl.copy(x = pl.x - 1)) else Set.empty
          val r = if (d == '^' | d == 'v') & grid(pr) == '[' then Set(pr.copy(x = pr.x + 1)) else Set.empty
          l ++ c ++ r

        def unblocked(ms: Set[(Pos,Pos)]): Boolean = ms.map(_._2).forall(isFree)
        def blocked(ms: Set[(Pos,Pos)]): Boolean   = ms.map(_._2).exists(isWall)

        val moves = widen(tos).map(from => from -> from.move(d))
        if unblocked(moves) then
          updates ++ moves
        else if blocked(moves) then
          Set.empty
        else
          loop(tos = widen(moves.map(_._2).filter(isBox)), updates = updates ++ moves)

      loop(Set(p))

    infix def push(d: Dir): Grid =
      val updates = moves(robot, d)
      if updates.isEmpty then
        this
      else
        val updated: Map[Pos,Char] =
          updates.foldLeft(grid):
            case (g,(f,t)) => g.updated(t, grid(f))

        val updatedAndCleaned: Map[Pos,Char] =
          val fs = updates.map(_._1)
          val ts = updates.map(_._2)
          fs.filter(f => !ts.contains(f)).foldLeft(updated)((g,p) => g.updated(p, '.'))

        Grid(updatedAndCleaned)

    def resize: Grid =
      Grid(
        grid.flatMap:
          case (p,'#') => Map(p.copy(x = p.x * 2) -> '#', p.copy(x = (p.x * 2) + 1) -> '#')
          case (p,'.') => Map(p.copy(x = p.x * 2) -> '.', p.copy(x = (p.x * 2) + 1) -> '.')
          case (p,'O') => Map(p.copy(x = p.x * 2) -> '[', p.copy(x = (p.x * 2) + 1) -> ']')
          case (p,'@') => Map(p.copy(x = p.x * 2) -> '@', p.copy(x = (p.x * 2) + 1) -> '.')
      )


  val (grid: Grid, moves: List[Dir]) =
    val Array(top, bottom) = Source.fromResource(s"input$day.txt").mkString.split("\n\n").map(_.trim)
    val positions = for {
      (l, y) <- top.split("\n").zipWithIndex
      (c, x) <- l.trim.zipWithIndex
    } yield Pos(x, y) -> c
    val moves = bottom.filterNot(_ == '\n').toList
    (Grid(positions.toMap), moves)

  val start1: Long  = System.currentTimeMillis
  val answer1: Int = moves.foldLeft(grid)(_ push _).boxes.map(_.coordinate).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = moves.foldLeft(grid.resize)(_ push _).boxes.map(_.coordinate).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


