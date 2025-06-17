import scala.io.Source
import scala.util.Try

object Day14 extends App:

  val day: String = this.getClass.getName.drop(3).init

  /** Modeling */

  enum E(val c: Char):
    case S extends E('o')
    case A extends E('.')
    case R extends E('#')

  case class Pos(x: Int, y: Int)

  object Pos:
    given Ordering[Pos] with
      def compare(a: Pos, b: Pos): Int =
        Ordering[(Int,Int)].compare((a.x, a.y), (b.x, b.y))

  def parse(s: String): List[Pos] =
    def loop(ps: List[String], a: List[Pos] = List.empty): List[Pos] =
      ps match
        case Nil           => a
        case s"$x,$y" :: t => loop(t, a :+ Pos(x.toInt,y.toInt))
        case _ => sys.error("boom!")
    loop(s.trim.split(""" -> """).toList)


  val rocks: List[Pos] =
    val paths =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(parse)
        .toList

    def segment(f: Pos, t: Pos): List[Pos] =
      (if     f.x == t.x && f.y < t.y then (f.y to t.y).map(y => Pos(t.x, y))
      else if f.x == t.x && f.y > t.y then (t.y to f.y).map(y => Pos(t.x, y))
      else if f.y == t.y && f.x < t.x then (f.x to t.x).map(x => Pos(x, t.y))
      else if f.y == t.y && f.x > t.x then (t.x to f.x).map(x => Pos(x, t.y))
      else sys.error("boom!")).toList

    def positions(path: List[Pos], a: List[Pos] = List.empty): List[Pos] =
      path match
        case _ :: Nil    => a
        case f :: t :: r => positions(t :: r, a :++ segment(f, t))
        case _           => sys.error("boom!")

    paths.foldLeft(List.empty[Pos])((a,p) => a :++ positions(p)).distinct


  case class Cave(view: List[List[E]], minX: Int):
    import E.*

    def get(p: Pos): Option[E] =
      view.lift(p.y).flatMap(_.lift(p.x - minX))

    def set(p: Pos): Cave =
      Cave(view.updated(p.y, view(p.y).updated(p.x - minX, S)), minX)

    def land(cur: Pos): (Pos,Boolean) =
      def find(p: Pos): Option[Pos] =
        val below: List[Pos] = List(Pos(p.x, p.y + 1), Pos(p.x - 1, p.y + 1), Pos(p.x + 1, p.y + 1))
        below.find(p => get(p) == None || get(p) == Some(A))
      find(cur) match
        case None                      => (cur, false)
        case Some(n) if get(n).isEmpty => (n, true)
        case Some(n)                   => land(n)

    def count: Int =
      view.flatten.count(_ == S)

    def solve1: Int =
      val (p, overflow) = land(Cave.drip)
      if overflow then count
      else set(p).solve1

    def solve2: Int =
      val (p, _) = land(Cave.drip)
      if p == Cave.drip then
        count + 1
      else
        val n = set(p)
        if n.count % 100000 == 0 then
          n.solve2
        else
          n.solve2

    def asString: String =
      "\n" + view.map(_.map(_.c).mkString("")).mkString("\n")

  object Cave:
    import E.*
    def from1(rs: List[Pos]): Cave =
      val minX: Int = rs.map(_.x).min
      val maxX: Int = rs.map(_.x).max
      val minY: Int = 0
      val maxY: Int = rs.map(_.y).max
      val view: List[List[E]] =
        val rows  = List.fill(maxY - minY + 1, maxX - minX + 1)(A)
        rs.foldLeft(rows)((r,p) => r.updated(p.y, r(p.y).updated(p.x - minX, R)))
      Cave(view, minX)

    def from2(rs: List[Pos]): Cave =
      val maxY: Int = rs.map(_.y).max
      val view: List[List[E]] =
        val rows  = List.fill(maxY + 1, 1000)(A)
        rs.foldLeft(rows)((r,p) => r.updated(p.y, r(p.y)
          .updated(p.x, R)))
          :+ List.fill(1000)(A) :+ List.fill(1000)(R)
      Cave(view, 0)

    val drip: Pos = Pos(500,0)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Cave.from1(rocks).solve1
  println(s"Answer AOC 2022 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Cave.from2(rocks).solve2
  println(s"Answer AOC 2022 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
