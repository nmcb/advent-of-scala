
import nmcb.predef.*
import scala.annotation.targetName

object Day03 extends App:

  case class Pos(x: Int, y: Int):

    @targetName("plus")
    infix def +(p: Pos): Pos =
      Pos(x + p.x, y + p.y)

    infix def manhattan(p: Pos): Int =
      (x - p.x).abs + (y - p.y).abs

    def neighbours: Seq[Pos] =
      Pos.adjacent.map(_ + this)
  
  object Pos:

    val zero: Pos =
      Pos(0, 0)

    val adjacent: Seq[Pos] =
      for
        y <- -1 to 1
        x <- -1 to 1
        if !(x == 0 && y == 0)
      yield
        Pos(x, y)

  def position(input: Int): Pos =
    val n    = Iterator.iterate(1)(_ + 2).findFirst(n => n * n >= input)
    val base = input - (n - 2) * (n - 2) - 1
    val size = n - 1
    val half = size / 2
    val quadrant = base / size
    val offset = base % size
    quadrant match
      case 0 => Pos(half, offset + 1 - half)
      case 1 => Pos(half - 1 - offset, half)
      case 2 => Pos(-half, half - 1 - offset)
      case 3 => Pos(offset + 1 - half, -half)

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = Pos.zero manhattan position(277678)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def spiralSquares(to: Int):Int =
    def loop(n: Int, squares: Map[Pos,Int] = Map(Pos(0,0) -> 1)): Int =
      val point  = position(n)
      val result = point.neighbours.flatMap(squares.get).sum
      if result > to then
        result
      else
        loop(n + 1, squares.updated(point, result))
    loop(2)

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = spiralSquares(277678)

  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
