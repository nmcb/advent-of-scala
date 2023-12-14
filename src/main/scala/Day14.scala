import scala.annotation.tailrec
import scala.collection.*
import scala.io.*

object Day14 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Grid(image: Vector[Vector[Char]]):

    override def toString: String =
      image.map(_.mkString).mkString("\n", "\n", "\n")

    private def roll(s: Vector[Char]): Vector[Char] =
      @tailrec def loop(todo: Vector[Char], acc: Vector[Char] = Vector.empty): Vector[Char] =
        if todo.isEmpty then
          acc
        else if todo.head == '#' then
          loop(todo.tail, acc :+ '#')
        else
          val section = todo.takeWhile(_ != '#')
          val rounds  = section.count(_ == 'O')
          val empties = section.count(_ == '.')
          val aligned = section.foldLeft(Seq.empty[Char])((a,p) => Seq.fill(rounds)('O') ++ Seq.fill(empties)('.'))
          loop(todo.drop(section.length), acc ++ aligned)

      loop(s)

    lazy val tiltN: Grid =
      Grid(image.transpose.map(roll).transpose)

    lazy val tiltE: Grid =
      Grid(image.map(_.reverse).map(roll).map(_.reverse))

    lazy val tiltS: Grid =
      Grid(image.transpose.map(_.reverse).map(roll).map(_.reverse).transpose)

    lazy val tiltW: Grid =
      Grid(image.map(roll))

    lazy val cycle: Grid =
      tiltN.tiltW.tiltS.tiltE

    lazy val load: Int =
      image.reverse.zipWithIndex.foldLeft(0):
        case (a,(r,i)) => a + r.count(_ == 'O') * (i + 1)

    def cycleInvariant: Grid =
      this


  lazy val grid: Grid =
    Grid(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(_.toVector)
        .toVector)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grid.tiltN.load
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  lazy val cycle    = CycleFinder.find(grid, _.cycle)(_.cycleInvariant)
  lazy val tail     = (1_000_000_000L - cycle.stemLength) % cycle.cycleLength
  lazy val simulate = cycle.stemLength + cycle.cycleLength + tail.toInt
  lazy val end      = (0 until simulate).foldLeft(grid)((g,i) => g.cycle)

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = end.load
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

  /** Utilities */

  case class Cycle[A](stemLength: Int, cycleLength: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A)

  object CycleFinder:

    import scala.collection._

    extension[A] (it: Iterator[A]) def zipWithPrev: Iterator[(Option[A], A)] =
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
        .map:
          case ((last, prev), idx) => (last, prev, trace.put(m(prev), (prev, idx)), idx)
        .collectFirst:
          case (Some(last), repeat, Some((prev, prevIdx)), idx) =>
            Cycle(
              stemLength = prevIdx,
              cycleLength = idx - prevIdx,
              cycleHead = prev,
              cycleLast = last,
              cycleHeadRepeat = repeat
            )

    def find[A, B](x0: A, f: A => A)(m: A => B): Cycle[A] =
      find(Iterator.iterate(x0)(f))(m).get
