import scala.annotation.*
import scala.io.*

object Day12 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  enum Dir:
    case N, E, S, W

  import Dir.*

  case class Pos(x: Int, y: Int):
    lazy val n: Pos = move(N)
    lazy val e: Pos = move(E)
    lazy val s: Pos = move(S)
    lazy val w: Pos = move(W)

    def move(d: Dir): Pos =
      d match
        case N => copy(y = y - 1)
        case E => copy(x = x + 1)
        case S => copy(y = y + 1)
        case W => copy(x = x - 1)

    def adjacent(pos: Pos): Option[Dir] =
      Dir.values.find(d => move(d) == pos)

    def neighbours: Set[Pos] =
      Set(n, e, s, w)

    override def toString: String =
      s"($x,$y)"

  type Perimeter = Set[(Pos,Dir)]

  object Perimeter:

    def of(pos: Pos): Perimeter =
      Set((pos, N), (pos, E), (pos, S), (pos, W))

    extension (perimeter: Perimeter) infix def +(pos: Pos): Perimeter =
      val keep = perimeter.filterNot((p,d) => p.adjacent(pos).contains(d))
      val add  = of(pos).filterNot((p,d) => perimeter.exists((a,_) => a == p.move(d)))
      keep ++ add

  import Perimeter.*

  case class Region(tree: Char, plots: Set[Pos], perimeter: Perimeter):
    assert(plots.size == 1 | plots.forall(p => plots.exists(p.neighbours.contains)))

    def area: Int =
      plots.size

    def perimeterLength: Int =
      perimeter.size

    def perimeterSides: Int =
      def segments(list: List[Int]): Int =
        @tailrec
        def loop(l: List[Int], last: Option[Int] = None, result: Int = 0): Int =
          l match
            case Nil                            => result
            case h :: t if last.contains(h - 1) => loop(t, Some(h), result)
            case h :: t                         => loop(t, Some(h), result + 1)
        loop(list.sorted)

      val fence = perimeter.groupMap(_._2)(_._1)
      val n = fence.get(N).map(_.toList.groupMap(_.y)(_.x)).getOrElse(sys.error("no fence: N"))
      val e = fence.get(E).map(_.toList.groupMap(_.x)(_.y)).getOrElse(sys.error("no fence: E"))
      val s = fence.get(S).map(_.toList.groupMap(_.y)(_.x)).getOrElse(sys.error("no fence: S"))
      val w = fence.get(W).map(_.toList.groupMap(_.x)(_.y)).getOrElse(sys.error("no fence: W"))

      List(n, e, s, w).foldLeft(0): (sides, parts) =>
        sides + parts.foldLeft(0):
          case (a,(_,l)) =>
            a + segments(l)

    def fencePrize: Int =
      area * perimeterLength

    def fencePrizeWithBulkDiscount: Int =
      area * perimeterSides

    infix def +(p: Pos): Region =
      copy(plots = plots + p, perimeter = perimeter + p)

    def contains(p: Pos): Boolean =
      plots.contains(p)

    override def toString: String =
      s"Plot($tree, ${plots.mkString("[",",","]")}, perimeter=${perimeter.size}"

  type Garden = Vector[Vector[Char]]

  object Garden:

    extension (g: Garden) def sizeX: Int =
      g.headOption.map(_.size).getOrElse(0)

    extension (g: Garden) def sizeY: Int =
      g.size

    extension (g: Garden) def within(p: Pos) =
      p.x >= 0 & p.y >= 0 & p.x < g.sizeX & p.y < g.sizeY

    extension (g: Garden) def tree(p: Pos): Option[Char] =
      Option.when(g.within(p))(g(p.y)(p.x))

    extension (g: Garden) def plots: Set[Pos] =
      for {
        (l, y) <- g.zipWithIndex.toSet
        (c, x) <- l.zipWithIndex
      } yield Pos(x, y)

    extension (g: Garden) def plot(pos: Pos): Region =

      val c = g.tree(pos).getOrElse(sys.error(s"out of bounds: $pos"))

      @tailrec
      def loop(todo: Set[Pos], result: Region = Region(c, Set(pos), perimeter = Perimeter.of(pos))): Region =
        if todo.isEmpty then result else
          val p = todo.head
          val t = todo - p
          if g.tree(p).contains(c) & !result.contains(p) then
            loop(t ++ p.neighbours.filter(p => g.within(p) & !result.contains(p)).toVector, result + p)
          else
            loop(t, result)

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
            val r = g.plot(p)
            loop(t, visited = visited ++ r.plots, result :+ r)

      loop(g.plots.toSet)

  val garden: Garden =
    Source.fromResource(s"input$day.txt").getLines.toVector.map(_.toVector)

  import Garden.*

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = garden.regions.map(_.fencePrize).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = garden.regions.map(_.fencePrizeWithBulkDiscount).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
