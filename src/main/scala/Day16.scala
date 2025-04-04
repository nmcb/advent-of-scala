import scala.io.Source

object Day16 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Programs = String

  object Programs:
    def init: Programs = "abcdefghijklmnop"

  extension (ps: Programs)

    def spin(len: Int): Programs =
      ps.takeRight(len) ++ ps.take(ps.size - len)

    def exchange(a: Int, b: Int): Programs =
      val pa = ps(a)
      val pb = ps(b)
      ps.updated(a, pb).updated(b, pa)

    def partner(pa: Char, pb: Char): Programs =
      val a = ps.indexOf(pa)
      val b = ps.indexOf(pb)
      exchange(a, b)

    infix def move(m: String): Programs =
      m match
        case s"s$len"    => spin(len.toInt)
        case s"x$a/$b"   => exchange(a.toInt, b.toInt)
        case s"p$pa/$pb" => partner(pa.head, pb.head)

    infix def dance(ms: Vector[String]): Programs =
      ms.foldLeft(ps)(_ move _)

  val moves: Vector[String] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .toVector


  val start1: Long    = System.currentTimeMillis
  val answer1: String = Programs.init.dance(moves)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  case class Cycle[A](stemSize: Int, cycleSize: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A)

  object Cycle:

    import scala.collection._

    extension [A](i: Iterator[A]) def zipWithPrev: Iterator[(Option[A], A)] =
      new AbstractIterator[(Option[A], A)]:

        private var prev: Option[A] =
          None

        override def hasNext: Boolean =
          i.hasNext

        override def next: (Option[A], A) =
          val cur = i.next
          val last = prev
          prev = Some(cur)
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
              stemSize = previousIndex,
              cycleSize = index - previousIndex,
              cycleHead = previous,
              cycleLast = last,
              cycleHeadRepeat = repeat
            )

    /** finds a cycle for given invariant which includes given initial */
    def find[A, B](initial: A, f: A => A)(invariant: A => B): Cycle[A] =
      find(Iterator.iterate(initial)(f))(invariant).get

  val cycle          = Cycle.find(Programs.init, _.dance(moves))(identity)
  val remaining      = 1000000000L % cycle.cycleSize
  val remainingDance = List.iterate(Programs.init, remaining.toInt + 1)(_.dance(moves)).last

  val start2: Long    = System.currentTimeMillis
  val answer2: String = remainingDance
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")