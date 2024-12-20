import scala.annotation.*
import scala.io.*

import nmcb.*
import Dir.*

object Day12 extends App:

  val day: String = getClass.getName.filter(_.isDigit)

  type Tree   = Char
  type Fence  = (Pos,Dir)
  type Fences = Set[Fence]

  extension (f: Fence)
    def pos: Pos = f._1
    def dir: Dir = f._2

    def adjacentTo(p: Pos): Boolean =
      f.pos.move(f.dir) == p

    def adjacentTo(ps: Set[Pos]): Boolean =
      ps.exists(f.adjacentTo)

  extension (fences: Fences)
    def add(pos: Pos): Fences =
      val keep = fences.filterNot(_.adjacentTo(pos))
      val add  = Fences.around(pos).filterNot(_.adjacentTo(fences.map(_.pos)))
      keep ++ add


  object Fences:

    def around(pos: Pos): Fences =
      Set((pos, N), (pos, E), (pos, S), (pos, W))

  case class Region(tree: Tree, plots: Set[Pos], fences: Fences):

    def area: Int =
      plots.size

    def fencesLength: Int =
      fences.size

    def fencesSides: Int =

      def countSides(positions: Set[Int]): Int =
        @tailrec
        def loop(l: List[Int], last: Option[Int] = None, result: Int = 0): Int =
          l match
            case Nil                            => result
            case h :: t if last.contains(h - 1) => loop(t, Some(h), result)
            case h :: t                         => loop(t, Some(h), result + 1)
        loop(positions.toList.sorted)

      def fencePositions(d: Dir, p: Map[Dir,Set[Pos]], g: Pos => Int, f: Pos => Int): Map[Int,Set[Int]] =
        p.get(d).map(_.groupMap(g)(f)).getOrElse(sys.error(s"no fence: $d"))

      val fence = fences.groupMap(_._2)(_._1)
      val n = fencePositions(N, fence, _.y, _.x)
      val e = fencePositions(E, fence, _.x, _.y)
      val s = fencePositions(S, fence, _.y, _.x)
      val w = fencePositions(W, fence, _.x, _.y)

      List(n, e, s, w).foldLeft(0): (total, groupedFencePositions) =>
        total + groupedFencePositions.foldLeft(0):
          case (sides, (_, alignedFencePositions)) =>
            sides + countSides(alignedFencePositions)

    def fencePrize: Int =
      area * fencesLength

    def fencePrizeWithBulkDiscount: Int =
      area * fencesSides

    def add(p: Pos): Region =
      copy(plots = plots + p, fences = fences.add(p))

    def contains(p: Pos): Boolean =
      plots.contains(p)


  extension (g: Grid[Tree])

    def regionOf(pos: Pos): Region =
      val tree = g.peek(pos)
      @tailrec
      def loop(todo: Set[Pos], region: Region): Region =
        if todo.isEmpty then
          region
        else
          val p = todo.head
          val t = todo.tail
          if !region.contains(p) && g.contains(p, tree) then
            loop(t ++ p.adjWithinGrid(g, e => !region.contains(e._1)), region.add(p))
          else
            loop(t, region)

      val r = Region(tree, Set(pos), fences = Fences.around(pos))
      loop(pos.adjWithinGrid(g, _ => true), r)

    def regions: Vector[Region] =
      @tailrec
      def loop(todo: Set[Pos], visited: Set[Pos] = Set.empty, result: Vector[Region] = Vector.empty): Vector[Region] =
        if todo.isEmpty then
          result
        else
          val p = todo.head
          val t = todo.tail
          if visited.contains(p) then
            loop(t, visited, result)
          else
            val r = g.regionOf(p)
            loop(t, visited = visited ++ r.plots, result :+ r)

      loop(g.positions)

  val garden: Grid[Tree] =
    Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = garden.regions.map(_.fencePrize).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = garden.regions.map(_.fencePrizeWithBulkDiscount).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
