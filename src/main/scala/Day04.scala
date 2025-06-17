import scala.annotation.*
import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Card(id: Int, winning: Vector[Int], mine: Vector[Int]):

    def scratch(score: Int => Int): Int =
      mine.foldLeft(0)((s,n) => if winning.contains(n) then if s == 0 then 1 else score(s) else s)

    def points: Int =
      scratch(_ * 2)

    def matching: Int =
      scratch(_ + 1)

  object Card:

    def fromString(s: String): Card =

      def ints(s: String): Vector[Int] =
        s.trim.split("\\s+").map(_.toInt).toVector

      s match
        case s"Card $id: $winning | $mine" => Card(id.trim.toInt, ints(winning), ints(mine))
        case _ => sys.error(s"unmatched: $s")

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

    def update(from: Int)(counts: Map[Int,Int], id: Int): Map[Int,Int] =
      counts.updatedWith(id)(_.map(_ + counts(from)))

    @tailrec
    def scratch(id: Int = 1, counts: Map[Int,Int] = cards.map((i,c) => i -> 1)): Int =
      if id > maxId then
        counts.view.values.sum
      else
        val next  = (id + 1 to id + cards(id).matching).foldLeft(counts)(update(id))
        scratch(id + 1, next)

    scratch()

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
