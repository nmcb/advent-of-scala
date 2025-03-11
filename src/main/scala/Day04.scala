import scala.collection.mutable
import scala.io.Source

object Day04 extends App:

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  def memo[K, V](initial: (K, V)*): mutable.Map[K, V] =
    mutable.Map.empty[K, V] ++ initial

  extension [K, V](cache: mutable.Map[K, V])
    def memoize(k: K)(v: => V): V =
      cache.getOrElseUpdate(k, v)

  val cache: mutable.Map[String,Map[Char,Int]] = memo()
  extension (s: String) def charCount: Map[Char,Int] =
    cache.memoize(s)(s.groupMapReduce(identity)(_ => 1)(_ + _))

  extension (l: String) infix def anagram(r: String): Boolean =
    l.charCount == r.charCount

  case class Passphrase(words: Seq[String]):

    def valid1: Boolean =
      words.size == words.distinct.size

    def valid2: Boolean =
      words.combinations(2).forall(ws => !(ws.head anagram ws.last))

  val input: Seq[Passphrase] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim.split("\\s+").toSeq)
      .map(Passphrase.apply)
      .toSeq

  val start1: Long = System.currentTimeMillis
  val answer1: Int = input.count(_.valid1)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = input.count(_.valid2)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


/*

object Day03:
  val adjacent = for y <- -1 to 1; x <- -1 to 1 if !(x == 0 && y == 0) yield (x, y)

  case class Point(x: Int, y: Int):
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
    def neighbours: Seq[Point] = adjacent.map(delta)

  def position(input: Int): Point =
    val n = Iterator.iterate(1)(_ + 2).dropWhile(n => n * n < input).next()
    val base = input - (n - 2) * (n - 2) - 1
    val size = n - 1
    val half = size / 2
    val quadrant = base / size
    val offset = base % size
    quadrant match
      case 0 => Point(half, offset + 1 - half)
      case 1 => Point(half - 1 - offset, half)
      case 2 => Point(-half, half - 1 - offset)
      case 3 => Point(offset + 1 - half, -half)
  end position

  def part1(input: Int): Int =
    val point = position(input)
    point.x.abs + point.y.abs

  def part2(input: Int): Int =
    def helper(n: Int, squares: Map[Point, Int]): Int =
      val point = position(n)
      val result = point.neighbours.flatMap(squares.get).sum
      if result > input then result
      else helper(n + 1, squares.updated(point, result))

    helper(2, Map(Point(0, 0) -> 1))
  end part2

  def main(args: Array[String]): Unit =
    val data = 312051
    println(part1(data))
    println(part2(data))

*/