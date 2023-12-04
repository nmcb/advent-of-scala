import scala.annotation.*
import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Card(id: Int, winning: Vector[Int], mine: Vector[Int]):

    def points: Int =
      mine.foldLeft(0)((s,n) => if winning.contains(n) then if s == 0 then 1 else s * 2 else s)
    def matching: Int =
      mine.foldLeft(0)((s,n) => if winning.contains(n) then if s == 0 then 1 else s + 1 else s)

  object Card:

    def fromString(s: String): Card =
      s match
        case s"Card $i: $w | $m" =>
          val id      = i.trim.toInt
          val winning = w.trim.split("\\s+").map(_.toInt).toVector
          val mine    = m.trim.split("\\s+").map(_.toInt).toVector
          Card(id, winning, mine)
        case _ =>
          sys.error(s"unmatched: $s")

  val cards: Map[Int,Card] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Card.fromString)
      .map(c => c.id -> c)
      .toMap

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    cards.view.values.map(_.points).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =

    val maxId: Int =
      cards.keys.max

    extension (counts: Map[Int,Int]) def add(id: Int, delta: Int): Map[Int,Int] =
      counts.updatedWith(id)(_.map(_ + delta))

    @tailrec
    def solve2(id: Int = 1, acc: Map[Int,Int] = cards.map((i,c) => i -> 1)): Int =
      if id > maxId then acc.view.values.sum else solve2(id + 1, cards(id).matching match
        case 0 => acc
        case n => (id + 1 to id + n).foldLeft(acc)((a,n) => if n <= maxId then a.add(n, acc(id)) else a))

    solve2()

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
