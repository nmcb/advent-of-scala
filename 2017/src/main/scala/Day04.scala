
import nmcb.predef.*
import scala.io.Source

object Day04 extends App:

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  val cache = memo[String, Map[Char,Int]]()
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
  lazy val answer1: Int = input.count(_.valid1)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = input.count(_.valid2)
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
