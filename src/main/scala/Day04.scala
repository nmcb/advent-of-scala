import scala.annotation.tailrec
import scala.io.*

object Day04 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Room(underlying: String):

    def name: String =
      underlying.takeWhile(c => !c.isDigit)

    def id: Int =
      underlying.filter(_.isDigit).toInt

    def checksum: String =
      underlying match
        case s"$nameAndId[$checksum]" => checksum
        case _                        => sys.error(s"no checksum: $underlying")

    def isValid: Boolean =

      type Chars = (Char,Int)

      extension (chars: Chars)
        def char: Char = chars._1
        def index: Int = chars._2

      given ordering: Ordering[Chars] =
        Ordering
          .by[Chars,Int](_.index)
          .reverse
          .orElse(Ordering.by[Chars,Int](_.char))

      val common: List[Char] =
        name
          .filterNot(_ == '-')
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .toList
          .sorted
          .map(_.char)
          .take(5)

      checksum.forall(common.contains)

    def decrypt: String =

      @tailrec
      def rotate(c: Char, i: Int): Char =
        if c > 'z' then rotate('a', i) else if i == 0 then c else rotate((c + 1).toChar, i - 1)

      name.map:
        case '-' => ' '
        case c   => rotate(c, id)


  object Room:

    def fromLine(s: String): Room =
      Room(s)

  val rooms: List[Room] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Room.fromLine)
      .toList

  val start1  = System.currentTimeMillis
  val answer1 = rooms.filter(_.isValid).map(_.id).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = rooms.find(_.decrypt.trim == "northpole object storage").get.id
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
