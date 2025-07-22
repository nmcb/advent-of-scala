import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val start1: Long =
    System.currentTimeMillis

  val input: List[(Char,Char)] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map { case s"$p1 $p2" => (p1.head,p2.head) }
      .toList

  def score1(p1: Char, p2: Char): Int =
    (p1,p2) match
      case ('A','X') => 1 + 3
      case ('A','Y') => 2 + 6
      case ('A','Z') => 3 + 0
      case ('B','X') => 1 + 0
      case ('B','Y') => 2 + 3
      case ('B','Z') => 3 + 6
      case ('C','X') => 1 + 6
      case ('C','Y') => 2 + 0
      case ('C','Z') => 3 + 3
      case _ => sys.error(s"illegal chars: ($p1,$p2)")

  val answer1: Int =
    input.map(score1).sum

  println(s"Answer AOC 2022 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  def score2(p1: Char, p2: Char): Int =
    (p1,p2) match
      case ('A','X') => 3 + 0
      case ('A','Y') => 1 + 3
      case ('A','Z') => 2 + 6
      case ('B','X') => 1 + 0
      case ('B','Y') => 2 + 3
      case ('B','Z') => 3 + 6
      case ('C','X') => 2 + 0
      case ('C','Y') => 3 + 3
      case ('C','Z') => 1 + 6
      case _ => sys.error(s"illegal chars: ($p1,$p2)")

  val answer2: Int =
    input.map(score2).sum

  println(s"Answer AOC 2022 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
