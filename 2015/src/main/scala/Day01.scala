import scala.io.*

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val commands: List[Char] =
    Source
      .fromResource(s"input$day.txt", getClass.getClassLoader)
      .mkString
      .trim
      .toList

  val answer1: Int =
    commands
      .foldLeft(0): (f,c) =>
        if c == '(' then
          f + 1
        else if c == ')' then
          f - 1
        else
          sys.error(s"unknown command: '$c'")
  
  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    commands
      .scanLeft(0): (f,c) =>
        if c == '(' then
          f + 1
        else if c == ')' then
          f - 1
        else
          sys.error(s"unknown command: '$c'")
      .zipWithIndex
      .find((f,_) => f == -1)
      .getOrElse(sys.error("not found"))
      ._2
  
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
