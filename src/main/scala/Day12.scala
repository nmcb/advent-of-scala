import scala.annotation.*
import scala.io.*

object Day12 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Tree = Char

  enum Dir:
    case N, E, S, W

  import Dir.*


  case class Pos(x: Int, y: Int):

    def move(d: Dir): Pos =
      d match
        case N => copy(y = y - 1)
        case E => copy(x = x + 1)
        case S => copy(y = y + 1)
        case W => copy(x = x - 1)

    def adjacent(pos: Pos): Option[Dir] =
      Dir.values.find(d => move(d) == pos)

    def neighbours: Set[Pos] =
      Set(move(N), move(E), move(S), move(W))


  type Perimeter = Set[(Pos,Dir)]

  object Perimeter:

    def of(pos: Pos): Perimeter =
      Set((pos, N), (pos, E), (pos, S), (pos, W))

    extension (perimeter: Perimeter) def add(pos: Pos): Perimeter =
      val keep = perimeter.filterNot((p,d) => p.adjacent(pos).contains(d))
      val add  = of(pos).filterNot((p,d) => perimeter.exists((a,_) => a == p.move(d)))
      keep ++ add

  import Perimeter.*


  case class Region(tree: Tree, plots: Set[Pos], perimeter: Perimeter):

    def area: Int =
      plots.size

    def perimeterLength: Int =
      perimeter.size

    def perimeterSides: Int =
      
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

      val fence = perimeter.groupMap(_._2)(_._1)
      val n = fencePositions(N, fence, _.y, _.x)
      val e = fencePositions(E, fence, _.x, _.y)
      val s = fencePositions(S, fence, _.y, _.x)
      val w = fencePositions(W, fence, _.x, _.y)

      List(n, e, s, w).foldLeft(0): (total, groupedFencePositions) =>
        total + groupedFencePositions.foldLeft(0):
          case (sides, (_, alignedFencePositions)) =>
            sides + countSides(alignedFencePositions)

    def fencePrize: Int =
      area * perimeterLength

    def fencePrizeWithBulkDiscount: Int =
      area * perimeterSides

    def add(p: Pos): Region =
      copy(plots = plots + p, perimeter = perimeter.add(p))

    def contains(p: Pos): Boolean =
      plots.contains(p)


  type Garden = Vector[Vector[Tree]]

  object Garden:

    extension (g: Garden) def sizeX: Int =
      g.headOption.map(_.size).getOrElse(0)

    extension (g: Garden) def sizeY: Int =
      g.size

    extension (g: Garden) def within(p: Pos) =
      p.x >= 0 & p.y >= 0 & p.x < g.sizeX & p.y < g.sizeY

    extension (g: Garden) def tree(p: Pos): Option[Tree] =
      Option.when(g.within(p))(g(p.y)(p.x))

    extension (g: Garden) def plots: Set[Pos] =
      for {
        (l, y) <- g.zipWithIndex.toSet
        (c, x) <- l.zipWithIndex
      } yield Pos(x, y)

    extension (g: Garden) def region(pos: Pos): Region =

      val tree = g.tree(pos).getOrElse(sys.error(s"out of bounds: $pos"))

      @tailrec
      def loop(todo: Set[Pos], region: Region = Region(tree, Set(pos), perimeter = Perimeter.of(pos))): Region =
        if todo.isEmpty then region else
          val p = todo.head
          val t = todo - p
          if g.tree(p).contains(tree) & !region.contains(p) then
            loop(t ++ p.neighbours.filter(p => g.within(p) & !region.contains(p)).toVector, region.add(p))
          else
            loop(t, region)

      loop(pos.neighbours.filter(p => g.within(p)))

    extension (g: Garden) def regions: Vector[Region] =
      @tailrec
      def loop(todo: Set[Pos], visited: Set[Pos] = Set.empty, result: Vector[Region] = Vector.empty): Vector[Region] =
        if todo.isEmpty then
          result
        else
          val p = todo.head
          val t = todo - p
          if visited.contains(p) then
            loop(t, visited, result)
          else
            val r = g.region(p)
            loop(t, visited = visited ++ r.plots, result :+ r)

      loop(g.plots)

  val garden: Garden =
    Source.fromResource(s"input$day.txt").getLines.toVector.map(_.toVector)

  import Garden.*

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = garden.regions.map(_.fencePrize).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = garden.regions.map(_.fencePrizeWithBulkDiscount).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
