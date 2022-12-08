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
    assert(heightOf.size == (maxX + 1) * (maxY + 1))

    def isBorder(p: Pos): Boolean =
      p.x == 0 || p.x == maxX | p.y == 0 || p.y == maxY

    def innerHeightsL(p: Pos): List[Int] =
      (for (x <- 0 to p.x) yield heightOf(Pos.of(x, p.y))).toList

    def innerHeightsR(p: Pos): List[Int] =
      (for (x <- p.x to maxX) yield heightOf(Pos.of(x, p.y))).toList.reverse

    def innerHeightsT(p: Pos): List[Int] =
      (for (y <- 0 to p.y) yield heightOf(Pos.of(p.x, y))).toList

    def innerHeightsB(p: Pos): List[Int] =
      (for (y <- p.y to maxY) yield heightOf(Pos.of(p.x, y))).toList.reverse

    private def visible(p: Pos): Boolean =
      def loop(todo: List[Int]): Boolean =
        todo match
          case  _ :: Nil                                     => true
          case h0 :: h1 :: t if h0  < h1                     => loop(h1 :: t)
          case h0 :: h1 :: t if h0 == h1 && h1 < heightOf(p) => loop(h1 :: t)
          case _                                             => false
      val vl = { val l = innerHeightsL(p) ; val v = loop(l) ; println(s"calc l - p=$p, l=$l -> v=$v") ; v }
      val vr = { val l = innerHeightsR(p) ; val v = loop(l) ; println(s"calc r - p=$p, l=$l -> v=$v") ; v }
      val vt = { val l = innerHeightsT(p) ; val v = loop(l) ; println(s"calc t - p=$p, l=$l -> v=$v") ; v }
      val vb = { val l = innerHeightsB(p) ; val v = loop(l) ; println(s"calc b - p=$p, l=$l -> v=$v") ; v }
      vl || vr || vt || vb

    val visibleSize: Int =
      heightOf.keys.toList.sorted.foldLeft(List.empty[Pos])((s,p) =>
        if      isBorder(p) then { println(s"       - p=$p, h=${heightOf(p)} - visible border") ; p :: s }
        else if visible(p)  then { println(s"       - p=$p, h=${heightOf(p)} - visible") ;  p :: s }
        else                     { println(s"       - p=$p, h=${heightOf(p)} - invisible") ; s }
      ).size


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


  println(grid.visibleSize)

  val start1: Long =
    System.currentTimeMillis

  val answer1: Long =
    666

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  val answer2 =
    666

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
