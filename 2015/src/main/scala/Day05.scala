import scala.io.*

object Day05 extends App:

  val day: String  = getClass.getName.filter(_.isDigit).mkString

  val strings: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val vowels  = "[aeiou].*[aeiou].*[aeiou]".r.unanchored
  val pair    = "(.)\\1".r.unanchored
  val naughty = "ab|cd|pq|xy".r.unanchored

  val start1: Long = System.currentTimeMillis
  val answer1: Int = strings.count(line => vowels.matches(line) && pair.matches(line) && !naughty.matches(line))
  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val twoPair = "(..).*\\1".r.unanchored
  val triple  = "(.).\\1".r.unanchored

  val start2: Long = System.currentTimeMillis
  val answer2: Int = strings.count(line => twoPair.matches(line) && triple.matches(line))
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
