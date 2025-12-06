import scala.io.*

object Day26 extends App:

  val day: String = this.getClass.getName.drop(3).init

  type LookAndSay = Vector[Char]

  def decompose(number: LookAndSay): Vector[LookAndSay] =
    def loop(number: LookAndSay, digits: Vector[LookAndSay]): Vector[LookAndSay] =
      number match
        case h +: t if digits.last.contains(h) => loop(t, digits.init :+ (digits.last :+ h))
        case h +: t                            => loop(t, digits :+ (Vector.empty :+ h))
        case _                                 => digits
    if number.isEmpty then Vector.empty else loop(number.tail, Vector(Vector(number.head)))

  def successor(n: LookAndSay): LookAndSay =
    decompose(n).flatMap(c => Vector(c.size.toString.head, c.head))


  val start1: Long = System.currentTimeMillis
  lazy val answer10 = Iterator.iterate(Vector('1'))(successor).take(10).toList.last.size
  println(s"Answer AOC 2022 day $day part 2: ${answer10} [${System.currentTimeMillis - start1}ms]")
