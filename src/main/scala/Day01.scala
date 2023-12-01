import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val values: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  def recover1(s: String): Int =
    val l = s.dropWhile(!_.isDigit).head.toString
    val r = s.reverse.dropWhile(!_.isDigit).head.toString
    (l + r).toInt

  val answer1: Int =
    values.map(recover1).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  def recover2(s: String): Int =
    def left(todo: String): String =
      todo match
        case s"one$tail"   => "1"
        case s"two$tail"   => "2"
        case s"three$tail" => "3"
        case s"four$tail"  => "4"
        case s"five$tail"  => "5"
        case s"six$tail"   => "6"
        case s"seven$tail" => "7"
        case s"eight$tail" => "8"
        case s"nine$tail"  => "9"
        case s"zero$tail"  => "0"
        case _ if todo.head.isDigit => todo.head.toString
        case _ => left(todo.tail)

    def right(todo: String): String =
      todo match
        case s"${init}one"   => "1"
        case s"${init}two"   => "2"
        case s"${init}three" => "3"
        case s"${init}four"  => "4"
        case s"${init}five"  => "5"
        case s"${init}six"   => "6"
        case s"${init}seven" => "7"
        case s"${init}eight" => "8"
        case s"${init}nine"  => "9"
        case s"${init}zero"  => "0"
        case _ if todo.last.isDigit => todo.last.toString
        case _ => right(todo.init)

    (left(s) + right(s)).toInt

  val answer2: Int =
    values.map(recover2).sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
