import scala.io.*

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  extension (ps: Set[Pos]) def pairAll: Set[(Pos, Pos)] =
    ps.toSeq.combinations(2).map(p => p(0) -> p(1)).toSet

  case class Pos(x: Int, y: Int):
    infix def +(that: Pos): Pos     = Pos(x + that.x, y + that.y)
    infix def -(that: Pos): Pos     = Pos(x - that.x, y - that.y)

  case class Grid(grid: Vector[String]):
    val sizeX = grid.head.length
    val sizeY = grid.length

    def within(p: Pos): Boolean =
      p.x >= 0 & p.y >= 0 & p.x < sizeX & p.y < sizeY

    val positions: Set[(Char,Pos)] =
      (for {
        (l, y) <- grid.zipWithIndex
        (c, x) <- l.zipWithIndex
        if c != '.'
      } yield c -> Pos(x, y)).toSet

    val pairs: Set[(Char,Set[(Pos, Pos)])] =
      positions
        .groupMap((c,p) => c)((c,p) => p)
        .map((c,ps) => c -> ps.pairAll)
        .toSet

    def createTwice(a: Pos, b: Pos): Set[Pos] =
      Set(a + a - b, b + b - a).filter(within)

    def antiNodesPart1: Set[Pos] =
      pairs.flatMap((c,ps) => ps.flatMap(createTwice))

    def createInline(a: Pos, b: Pos): Set[Pos] =
      def loop(todo: Set[(Pos,Pos)], result: Set[Pos] = Set.empty): Set[Pos] =
        val found = todo.flatMap(createTwice) ++ result + a + b
        if found.size == result.size then result else loop(found.pairAll, found)
      loop(Set((a, b)))

    def antiNodesPart2: Set[Pos] =
      pairs.flatMap((c,ps) => ps.flatMap(createInline))

  val input: Grid =
    Grid(Source.fromResource(s"input$day.txt").getLines.toVector)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.antiNodesPart1.size
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = input.antiNodesPart2.size
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
