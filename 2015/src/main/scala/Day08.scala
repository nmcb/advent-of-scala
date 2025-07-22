import scala.annotation.*
import scala.io.*

object Day08 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  /** Modeling */

  case class Str(quoted: String):

    def unquoted: String =
      quoted.drop(1).dropRight(1)

    def unescaped: String =
      def loop(todo: List[Char], ret: String = ""): String =
        todo match
          case Nil =>
            ret
          case '\\' :: '\\' :: rest =>
            loop(rest, ret + '\\')
          case '\\' :: '\"' :: rest =>
            loop(rest, ret + '\"')
          case '\\' ::  'x' :: h1 :: h2 :: rest =>
            loop(rest, ret + Integer.parseInt(s"$h1$h2", 16).toChar)
          case c1 :: rest =>
            loop(rest, ret + c1)

      loop(unquoted.toList)

    def escaped: String =
      def loop(todo: List[Char], ret: String = ""): String =
        todo match
          case Nil          => "\"" + ret + "\""
          case '\"' :: rest => loop(rest, ret + "\\\"")
          case '\\' :: rest => loop(rest, ret + "\\\\")
          case   c1 :: rest => loop(rest, ret + c1)

      loop(quoted.toList)

  val input: IndexedSeq[Str] =
    def parser(s: String): Str =
      s match
        case s"""$unescaped""" => Str(unescaped)
        case str: String => sys.error(s"could not parse '$str'")

    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .toIndexedSeq


  /** Part 1 */

  val start1: Long = System.currentTimeMillis
  val answer1: Int = input.map(str => str.quoted.size - str.unescaped.size).sum

  println(s"Answer AOC 2015 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  val answer2: Int = input.map(str => str.escaped.size - str.quoted.size).sum

  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
