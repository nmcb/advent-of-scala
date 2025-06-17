import scala.collection.*
import scala.io.Source

object Day18 extends App:

  case class Pos(x: Int, y: Int):

    infix def+(that: Pos): Pos =
      copy(x = x + that.x, y = y + that.y)

    def adjacent: Set[Pos] =
      Set(Pos(-1,-1),Pos(-1,0),Pos(-1,1),Pos(0,-1),Pos(0,1),Pos(1,-1),Pos(1,0),Pos(1,1))
        .map(_ + this)

  type Area = Map[Pos,Char]

  case class Landscape(area: Area, sizeX: Int, sizeY: Int):

    def asString: String =
      val sb = mutable.StringBuilder()
      for
        y <- 0 until sizeX
        x <- 0 until sizeX
        p = Pos(x,y)
      do
        sb.append(area(p))
      sb.toString.grouped(sizeX).mkString("\n","\n","\n")

    def wooded: Int =
      area.view.values.count(_ == '|')

    def lumberyards: Int =
      area.view.values.count(_ == '#')

    def resourceValue: Int =
      wooded * lumberyards

    def within(p: Pos): Boolean =
      p.x >= 0 && p.x < sizeX && p.y >= 0 && p.y < sizeY

    def surroundedBy(p: Pos, c: Char): Int =
      p.adjacent.filter(within).count(p => area(p) == c)

    def tick: Landscape =

      val next = mutable.ArrayBuffer.empty[(Pos,Char)]
      for
        y <- 0 until sizeY
        x <- 0 until sizeX
        p = Pos(x,y)
      do
        val c = area(p) match
          case '.' => if surroundedBy(p, '|') >= 3 then '|' else '.'
          case '|' => if surroundedBy(p, '#') >= 3 then '#' else '|'
          case '#' => if surroundedBy(p, '#') >= 1 && surroundedBy(p, '|') >= 1 then '#' else '.'

        next += (p -> c)

      copy(area = next.toMap)


  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val landscape: Landscape =
    val lines = Source.fromResource(s"input$day.txt").getLines.toVector
    val sizeX = lines(0).size
    val sizeY = lines.size
    val area  = List.tabulate(sizeX, sizeY)((x,y) => Pos(x,y) -> lines(y)(x)).flatten.toMap
    Landscape(area, sizeX, sizeY)

  val start1  = System.currentTimeMillis
  val answer1 = Iterator.iterate(landscape)(_.tick).drop(10).next.resourceValue
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  case class Cycle[A](stemSize: Int, cycleSize: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A, next: A => A):

    def simulate(n: Long): A =
      val iterations = (n - stemSize) % cycleSize
      Iterator.iterate(cycleHeadRepeat)(next).drop(iterations.toInt).next


  object Cycle:

    import scala.collection.*

    extension [A](i: Iterator[A])
      private def zipWithPrevious: Iterator[(A,Option[A])] =
        new AbstractIterator[(A,Option[A])]:
  
          private var last: Option[A] =
            None
  
          override def hasNext: Boolean =
            i.hasNext
  
          override def next: (A,Option[A]) =
            val current  = i.next
            val previous = last
            last = Some(current)
            (current, previous)

    def find[A,B](a: A)(next: A => A, invariant: A => B = identity): Cycle[A] =

      val trace: mutable.Map[B,(A,Int)] = mutable.Map[B,(A,Int)]()

      Iterator.iterate(a)(next)
        .iterator
        .zipWithPrevious
        .zipWithIndex
        .map:
          case ((current, previous), index) =>
            (current, previous, trace.put(invariant(current), (current,index)), index)
        .collectFirst:
          case (current, Some(previous), Some(first,firstIndex), index) =>
            Cycle(
              stemSize        = firstIndex,
              cycleSize       = index - firstIndex,
              cycleHead       = first,
              cycleLast       = previous,
              cycleHeadRepeat = current,
              next            = next
            )
        .get

  val start2  = System.currentTimeMillis
  val answer2 = Cycle.find(landscape)(_.tick).simulate(1000000000).resourceValue
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
