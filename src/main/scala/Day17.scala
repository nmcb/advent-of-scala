import scala.annotation.*
import scala.collection.AbstractIterator
import scala.io.*
import scala.math.*
import scala.math.Integral.Implicits.*

object Day17 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val input: List[Move] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .toList

  case class Pos(x: Int, y: Int):
    def -(that: Pos): Pos = Pos(x - that.x, y - that.y)
    def translate(dx: Int, dy: Int): Pos = Pos(x + dx, y + dy)

  object Pos:
    val origin: Pos = Pos(0,0)


  type Move = Char
  val L = '<'
  val R = '>'
  val D = 'v'

  object Move:

    val pattern: LazyList[Move] =
      input.to(LazyList) #::: pattern


  sealed abstract class Rock(val relative: List[Pos]):
    def withOrigin(o: Pos): List[Pos] = relative.map(p => Pos(p.x + o.x, p.y + o.y))

  case object Min   extends Rock(List(Pos(0,0),Pos(1,0),Pos(2,0),Pos(3,0)))
  case object Plus  extends Rock(List(Pos(1,0),Pos(0,1),Pos(1,1),Pos(2,1),Pos(1,2)))
  case object El    extends Rock(List(Pos(0,0),Pos(1,0),Pos(2,0),Pos(2,1),Pos(2,2)))
  case object Stack extends Rock(List(Pos(0,0),Pos(0,1),Pos(0,2),Pos(0,3)))
  case object Box   extends Rock(List(Pos(0,0),Pos(1,0),Pos(0,1),Pos(1,1)))

  object Rock:
    val sequence: LazyList[Rock] = LazyList(Min, Plus, El, Stack, Box) #::: sequence


  case class Chamber(rocks: LazyList[Rock], pattern: LazyList[Move], stopped: Set[Pos], height: Int):
    import Chamber.*

    def isWall(p: Pos): Boolean     = p.x < Pos.origin.x | p.x >= Pos.origin.x + width
    def isFloor(p: Pos): Boolean    = p.y < Pos.origin.y
    def isOccupied(p: Pos): Boolean = isWall(p) | isFloor(p) | stopped.contains(p)

    def next: Chamber =

      def appear: Pos =
        Pos.origin.translate(dx = 2, dy = height + 3)

      def drop(pos: Pos, moves: LazyList[Move], trace: List[Move] = List.empty): (List[Move], Pos) =

        val moved = moves.head match
          case L => pos.translate(dx = -1, dy =  0)
          case R => pos.translate(dx =  1, dy =  0)
          case D => pos.translate(dx =  0, dy = -1)

        if rocks.head.withOrigin(moved).forall(p => !isOccupied(p)) then
          if moves.head == L || moves.head == R then
            drop(moved, D #:: moves.tail, moves.head :: trace)
          else
            drop(moved, moves.tail, moves.head :: trace)
        else
          if moves.head == L || moves.head == R then
            drop(pos, D #:: moves.tail, moves.head :: trace)
          else
            (trace, pos)

      val (moves, pos) = drop(appear, pattern)

      val dropped: Set[Pos] =
        stopped ++ rocks.head.withOrigin(pos)

      val maxY: Int =
        dropped.map(_.y).max + 1

      val consumed: LazyList[Move] =
        pattern.drop(moves.filterNot(_ == D).size)

      Chamber(rocks = rocks.tail, pattern = consumed, stopped = dropped.filter(_.y >= maxY - slidingWindow), height = maxY)

    def cycleInvariant: Set[Pos] =
      stopped.map(_ - Pos(0, height - slidingWindow))


  object Chamber:

    val slidingWindow: Int =
      100

    val width: Int =
      7

    def empty: Chamber =
      Chamber(Rock.sequence, Move.pattern, Set.empty, 0)


  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    Iterator.iterate(Chamber.empty)(_.next).drop(2022).next.height

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =

    val cycle: Cycle[Chamber] =
      CycleFinder.find(Chamber.empty, _.next)(_.cycleInvariant)

    val (cycleNr, tailNr) =
      (1000_000_000_000L - cycle.stemLength) /% cycle.cycleLength

    val stemHeight: Long =
      cycle.cycleHead.height

    val cycleHeight: Long =
      cycle.cycleHeadRepeat.height - stemHeight

    val tailHeight: Long =
      Iterator.iterate(cycle.cycleHead)(_.next).drop(tailNr.toInt).next.height - stemHeight

    stemHeight + cycleNr * cycleHeight + tailHeight

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


  /** Utilities */

  case class Cycle[A](stemLength: Int, cycleLength: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A)

  object CycleFinder:

    import scala.collection._

    extension [A](it: Iterator[A]) def zipWithPrev: Iterator[(Option[A], A)] =
        new AbstractIterator[(Option[A], A)]:

          private var prevOption: Option[A] =
            None

          override def hasNext: Boolean =
            it.hasNext

          override def next: (Option[A], A) =
            val cur = it.next
            val ret = (prevOption, cur)
            prevOption = Some(cur)
            ret

    def find[A, B](coll: IterableOnce[A])(m: A => B): Option[Cycle[A]] =

      val trace: mutable.Map[B, (A, Int)] =
        mutable.Map[B, (A, Int)]()

      coll.iterator
        .zipWithPrev
        .zipWithIndex
        .map({ case ((last, prev), idx) => (last, prev, trace.put(m(prev), (prev, idx)), idx) })
        .collectFirst({ case (Some(last), repeat, Some((prev, prevIdx)), idx) =>
          Cycle(
            stemLength      = prevIdx,
            cycleLength     = idx - prevIdx,
            cycleHead       = prev,
            cycleLast       = last,
            cycleHeadRepeat = repeat
          )
        })

    def find[A, B](x0: A, f: A => A)(m: A => B): Cycle[A] =
      find(Iterator.iterate(x0)(f))(m).get






