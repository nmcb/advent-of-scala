import scala.io.*

object Day02 extends App:

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  val input: Seq[Seq[Int]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim.split("\\s+").map(_.toInt).toSeq)
      .toSeq

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = input.map(ns => ns.max - ns.min).sum
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def process(ns: Seq[Int]): Int =
    val Seq(denominator, nominator) = ns.combinations(2).map(_.sorted).find(n => n(1) % n(0) == 0).get
    nominator / denominator

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = input.map(process).sum
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
