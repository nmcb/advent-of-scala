import Day05.moves

import scala.io.*
import scala.util.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Move(size: Int, from: Int, to: Int)

  val input: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString

  def solve(s: String, d: Int): Int =
    def loop(todo: String, last: String, count: Int = d): Int =
      if todo.isEmpty then sys.error("not found")
      else
        val c = todo.head
        val l = (last + c).drop(1)
        if   count >= d && l.distinct.size == d then count + 1
        else loop(todo.tail, l, count + 1)
    loop(s.drop(d), s.take(d))

  def solve1(s: String): Int =
    solve(s, 4)

  def solve2(s: String): Int =
    solve(s, 14)

  assert(solve1("mjqjpqmgbljsphdztnvjfqwrcgsmlb")    ==  7)
  assert(solve1("bvwbjplbgvbhsrlpgdmjqwftvncz")      ==  5)
  assert(solve1("nppdvjthqldpwncqszvftbrmjlhg")      ==  6)
  assert(solve1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 10)
  assert(solve1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  == 11)

  val answer1: Int =
    solve1(input)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  assert(solve2("mjqjpqmgbljsphdztnvjfqwrcgsmlb")    == 19)
  assert(solve2("bvwbjplbgvbhsrlpgdmjqwftvncz")      == 23)
  assert(solve2("nppdvjthqldpwncqszvftbrmjlhg")      == 23)
  assert(solve2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 29)
  assert(solve2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  == 26)

  val answer2: Int =
    solve2(input)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
