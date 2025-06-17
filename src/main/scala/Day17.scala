import java.security.MessageDigest
import scala.collection.*
import scala.annotation.tailrec

object Day17 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString
  val md5 = MessageDigest.getInstance("MD5")
  val hex = "0123456789abcdef".toCharArray

  extension (bytes: Array[Byte])
    def toHexString: String =
      val sb: StringBuffer = StringBuffer(bytes.length * 2)
      for b <- bytes do
        sb.append(hex((b & 0xF0) >> 4))
        sb.append(hex(b & 0x0F))
      sb.toString

  case class Pos(x: Int, y: Int):

    def move(direction: Char): Pos =
      direction match
        case 'U' => copy(y = y - 1)
        case 'D' => copy(y = y + 1)
        case 'L' => copy(x = x - 1)
        case 'R' => copy(x = x + 1)


  /** hardcoded for 4x4 grid */
  case class Path(passcode: String, path: String = "", target: Pos = Pos(0, 0)):

    def withinGrid: Boolean =
      target.x >= 0 && target.x < 4 && target.y >= 0 && target.y < 4

    def reachedVault: Boolean =
      target == Pos(3, 3)

    def openDoors: String =
      md5
        .digest((passcode + path)
        .getBytes)
        .toHexString
        .take(4)
        .zip("UDLR")
        .foldLeft(""):
          case (result, (char, direction)) =>
            if "bcdef".contains(char) then result + direction else result

    def candidates: Vector[Path] =
      openDoors
        .foldLeft(Vector.empty[Path]): (result, direction) =>
          result :+ copy(path = path + direction, target = target.move(direction))
        .filter(_.withinGrid)

    /** breadth first search - return first reached */
    def solve1: Option[Path] =
      val todo    = mutable.Queue(this)
      var current = todo.dequeue

      while !current.reachedVault do
        current.candidates.foreach(todo.enqueue)
        if todo.isEmpty then return None
        current = todo.dequeue

      Some(current)

    /** breadth first search - return last reached */
    def solve2: Option[Path] =
      val todo   = mutable.Queue(this)
      var result = Option.empty[Path]

      while todo.nonEmpty do
        val current = todo.dequeue
        if current.reachedVault then
          result = Some(current)
        else
          current.candidates.foreach(todo.enqueue)

      result

  val start1  = System.currentTimeMillis
  val answer1 = Path(passcode = "vwbaicqe").solve1.map(_.path).getOrElse("<none>")
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = Path(passcode = "vwbaicqe").solve2.map(_.path.length).getOrElse(-1)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
