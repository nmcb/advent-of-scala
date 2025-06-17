import scala.io.Source

object Day21 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Operation:
    case SwapPosition(x: Int, y: Int)
    case SwapLetter(a: Char, b: Char)
    case Rotate(d: String, n: Int)
    case RotateByPosition(a: Char)
    case ReverseByPosition(x: Int, y: Int)
    case Move(x: Int, y: Int)

    extension (s: String)

      def swapPosition(x: Int, y: Int): String =
        s.updated(x, s(y)).updated(y, s(x))

      def swapLetter(a: Char, b: Char): String =
        val (x, y) = (s.indexOf(a), s.indexOf(b))
        s.updated(x, s(y)).updated(y, s(x))

      def rotateRight(n: Int): String =
        s.takeRight(n) + s.dropRight(n)

      def rotateLeft(n: Int): String =
        s.drop(n) + s.take(n)

      def rotateByPositionRight(a: Char): String =
        val index = s.indexOf(a)
        val n = if index >= 4 then (index + 2) % s.length else (index + 1) % s.length
        s.takeRight(n) + s.dropRight(n)

      def rotateByPositionLeft(a: Char): String =
        Iterator.iterate(s)(_.rotateLeft(1)).dropWhile(_.rotateByPositionRight(a) != s).next

      def reverseByPosition(x: Int, y: Int): String =
        s.patch(x, s.slice(x, y + 1).reverse, y + 1 - x)

      def move(x: Int, y: Int): String =
        s.patch(x, "", 1).patch(y, s.charAt(x).toString, 0)

    infix def scramble(s: String): String =
      this match
        case SwapPosition(x, y)      => s.swapPosition(x, y)
        case SwapLetter(a, b)        => s.swapLetter(a, b)
        case Rotate("left", n)       => s.rotateLeft(n)
        case Rotate("right", n)      => s.rotateRight(n)
        case RotateByPosition(a)     => s.rotateByPositionRight(a)
        case ReverseByPosition(x, y) => s.reverseByPosition(x, y)
        case Move(x, y)              => s.move(x, y)
        case _                       => sys.error("boom!")

    infix def unscramble(s: String): String =
      this match
        case SwapPosition(x, y)      => s.swapPosition(x, y)
        case SwapLetter(a, b)        => s.swapLetter(a, b)
        case Rotate("left", n)       => s.rotateRight(n)
        case Rotate("right", n)      => s.rotateLeft(n)
        case RotateByPosition(a)     => s.rotateByPositionLeft(a)
        case ReverseByPosition(x, y) => s.reverseByPosition(x, y)
        case Move(x, y)              => s.move(y, x)
        case _                       => sys.error("boom!")

  import Operation.*


  val operations: Vector[Operation] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"swap position $x with position $y"     => SwapPosition(x.toInt, y.toInt)
        case s"swap letter $a with letter $b"         => SwapLetter(a.head, b.head)
        case s"rotate based on position of letter $a" => RotateByPosition(a.head)
        case s"rotate $d $n $plural"                  => Rotate(d, n.toInt)
        case s"reverse positions $x through $y"       => ReverseByPosition(x.toInt, y.toInt)
        case s"move position $x to position $y"       => Move(x.toInt, y.toInt)
        case s: String => sys.error(s"match error: '$s'")
      .toVector


  val start1  = System.currentTimeMillis
  val answer1 = operations.foldLeft("abcdefgh")((s,o) => o.scramble(s))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = operations.foldRight("fbgdceah")((o,s) => o.unscramble(s))
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
