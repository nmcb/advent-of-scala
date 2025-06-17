import scala.annotation.*
import scala.io.*

object Day09 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Circle[A](init: List[A], current: A, tail: List[A]):

    private def next: Circle[A] =
      tail match
        case hd :: tl => Circle(current :: init, hd, tl)
        case Nil      => init.reverse match
                           case hd :: it => Circle(List(current), hd, it)
                           case Nil      => this

    private def prev: Circle[A] =
      init match
        case hd :: it => Circle(it, hd, current :: tail)
        case Nil      => tail.reverse match
                           case hd :: tl => Circle(tl, hd, List(current))
                           case Nil      => this

    @tailrec
    final def rotate(n: Int): Circle[A] =
      if n > 0 then next.rotate(n - 1) else if n < 0 then prev.rotate(n + 1) else this

    def insert(elem: A): Circle[A] =
      Circle(init, elem, current :: tail)

    def removed: (A, Circle[A]) =
      tail match
        case hd :: tl => (current, Circle(init, hd, tl))
        case Nil      => (current, Circle(init.tail, init.head, tail))

  object Circle:
    def apply[A](elem: A): Circle[A] =
      Circle(Nil, elem, Nil)


  def play(players: Int, last: Int): Long =
    @tailrec
    def go(marbles: Circle[Int], marble: Int, player: Int, scores: Map[Int, Long]): Map[Int, Long] =

      val p: Int = player % players + 1

      if marble > last then
        scores
      else if marble % 23 != 0 then
        go(marbles.rotate(2).insert(marble), marble + 1, p, scores)
      else
        val (r, next) = marbles.rotate(-7).removed
        val s = scores.updated(player, scores(player) + marble + r)
        go(next, marble + 1, p, s)

    go(Circle(0), 1, 1, Map.empty.withDefaultValue(0)).values.max

  def solve(players: Int, last: Int, multiplier: Int = 1): Long =
    play(players, last * multiplier)

  val (players, points) =
    Source.fromResource(s"input$day.txt").mkString.trim match
      case s"$players players; last marble is worth $points points" => (players.toInt, points.toInt)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve(players, points)
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve(players, points, 100)
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
