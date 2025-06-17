import scala.annotation.tailrec
import scala.collection.*
import scala.io.*

object Day14 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Grid(image: Vector[Vector[Char]]):

    override def toString: String =
      image.map(_.mkString).mkString("\n", "\n", "\n")

    private def tiltL(row: Vector[Char]): Vector[Char] =
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

      loop(row)

    lazy val tiltN: Grid =
      Grid(image.transpose.map(tiltL).transpose)

    lazy val tiltE: Grid =
      Grid(image.map(l => tiltL(l.reverse).reverse))

    lazy val tiltS: Grid =
      Grid(image.transpose.map(l => tiltL(l.reverse).reverse).transpose)

    lazy val tiltW: Grid =
      Grid(image.map(tiltL))

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
  println(s"Answer AOC 2023 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  lazy val cycle = Cycle.find(grid, _.cycle)(_.cycleInvariant)
  lazy val tail  = (1_000_000_000L - cycle.stemSize) % cycle.cycleSize
  lazy val count = cycle.stemSize + tail.toInt
  lazy val end   = (0 until count).foldLeft(grid)((g, _) => g.cycle)

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = end.load
  println(s"Answer AOC 2023 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

  /** Utilities */

  case class Cycle[A](stemSize: Int, cycleSize: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A)

  object Cycle:

    import scala.collection.*

    extension[A] (i: Iterator[A]) def zipWithPrev: Iterator[(Option[A], A)] =
      new AbstractIterator[(Option[A], A)]:

        private var prev: Option[A] =
          None

        override def hasNext: Boolean =
          i.hasNext

        override def next: (Option[A], A) =
          val cur  = i.next
          val last = prev
          prev     = Some(cur)
          (last, cur)

    def find[A, B](sequence: IterableOnce[A])(invariant: A => B): Option[Cycle[A]] =

      val trace: mutable.Map[B, (A, Int)] =
        mutable.Map[B, (A, Int)]()

      sequence
        .iterator
        .zipWithPrev
        .zipWithIndex
        .map:
          case ((last, previous), index) =>
            (last, previous, trace.put(invariant(previous), (previous, index)), index)
        .collectFirst:
          case (Some(last), repeat, Some((previous, previousIndex)), index) =>
            Cycle(
              stemSize        = previousIndex,
              cycleSize       = index - previousIndex,
              cycleHead       = previous,
              cycleLast       = last,
              cycleHeadRepeat = repeat
            )

    def find[A, B](x0: A, f: A => A)(m: A => B): Cycle[A] =
      find(Iterator.iterate(x0)(f))(m).get
