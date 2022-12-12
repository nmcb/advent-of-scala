import scala.io.Source

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int)

  object Pos:
    def of(x: Int, y: Int): Pos =
      Pos(x,y)

    given Ordering[Pos] with
      def compare(a: Pos, b: Pos): Int =
        Ordering[(Int,Int)].compare((a.y, a.x), (b.y, b.x))

  case class Mat(heightOf: Map[Pos,Int]):

    val maxX: Int = heightOf.keys.map(_.x).max
    val maxY: Int = heightOf.keys.map(_.y).max

    def heightOf(p: (Int, Int)): Int = heightOf(Pos.of(p._1, p._2))

    def isBorder(p: Pos): Boolean = p.x == 0 || p.x == maxX | p.y == 0 || p.y == maxY

    def visible(p: Pos): Boolean =
      def pathL(p: Pos): List[Int] = (0 to p.x).map(x    => heightOf(Pos.of(x, p.y))).toList
      def pathR(p: Pos): List[Int] = (p.x to maxX).map(x => heightOf(Pos.of(x, p.y))).toList.reverse
      def pathT(p: Pos): List[Int] = (0 to p.y).map(y    => heightOf(Pos.of(p.x, y))).toList
      def pathB(p: Pos): List[Int] = (p.y to maxY).map(y => heightOf(Pos.of(p.x, y))).toList.reverse
      def visible(todo: List[Int]): Boolean = todo.init.max < todo.last
      val vl = visible(pathL(p))
      val vr = visible(pathR(p))
      val vt = visible(pathT(p))
      val vb = visible(pathB(p))
      vl || vr || vt || vb

    def maxVisibleTrees: Int =
      heightOf.keys.foldLeft(List.empty[Pos])((s,p) =>
        if      isBorder(p) then p :: s
        else if visible(p)  then p :: s
        else                          s
      ).size

    def scenicScore(p: Pos): Int =
      def pathL(p: Pos): List[Int] = (0 to p.x).map(x    => heightOf(Pos.of(x, p.y))).toList.reverse
      def pathR(p: Pos): List[Int] = (p.x to maxX).map(x => heightOf(Pos.of(x, p.y))).toList
      def pathT(p: Pos): List[Int] = (0 to p.y).map(y    => heightOf(Pos.of(p.x, y))).toList.reverse
      def pathB(p: Pos): List[Int] = (p.y to maxY).map(y => heightOf(Pos.of(p.x, y))).toList
      def visible(todo: List[Int]): Int =
        val index  = todo.tail.indexWhere(_ >= todo.head)
        if index != -1 then index + 1 else todo.tail.length
      val vl = visible(pathL(p))
      val vr = visible(pathR(p))
      val vt = visible(pathT(p))
      val vb = visible(pathB(p))
      vl * vr * vt * vb

    def maxScenicScore: Int =
      heightOf.keys.map(scenicScore).max

  val grid: Mat =
    Mat(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .zipWithIndex
        .foldLeft(Map.empty[Pos,Int]) { case (a,(r,y)) =>
          r.zipWithIndex.foldLeft(a){ case (a,(t,x)) =>
            a + (Pos(x,y) -> t.toString.toInt)
        }})


  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grid.maxVisibleTrees
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = grid.maxScenicScore
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
