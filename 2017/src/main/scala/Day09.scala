import scala.io.Source

object Day09 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  private var garbage: Boolean = false
  private var escape: Boolean  = false

  private var depth: Int   = 0
  private var score: Int   = 0
  private var counter: Int = 0

  Source
    .fromResource(s"input$day.txt")
    .mkString
    .trim
    .foreach: char =>
      if garbage then
        if escape then
          escape = false
        else if char == '>' then
          garbage = false
        else if char == '!' then
          escape = true
        else
          counter += 1
      else if char == '{' then
        depth += 1
        score += depth
      else if char == '}' then
        depth -= 1
      else if char == '<' then
        garbage = true

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = score
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = counter
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
