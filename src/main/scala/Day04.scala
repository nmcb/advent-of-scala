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

  val input: Vector[Card] =
    Source.fromResource(s"input$day.txt").getLines.map(Card.fromString).toVector

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    input.map(_.points).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  def scratch(deck: Vector[Card]): Int =

    val lookup: Map[Int,Card] =
      deck.map(c => c.id -> c).toMap

    val maxId: Int =
      deck.map(_.id).max

    extension (counts: Map[Int, Int]) def add(id: Int, delta: Int): Map[Int, Int] =
      counts.updatedWith(id)(_.map(_ + delta))

    def loop(id: Int, acc: Map[Int,Int]): Int =
      if id > maxId then acc.view.values.sum else loop(id + 1, lookup(id).matching match
        case 0 => acc
        case n => (id + 1 to id + n).foldLeft(acc)((a, n) => if n <= maxId then a.add(n, acc(id)) else a)
      )

    loop(1, deck.map(c => c.id -> 1).toMap)

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    scratch(input)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
