import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Color = String

  case class Hand(cubes: Map[Color,Long]):
    val reds: Long   = cubes.getOrElse("red", 0L)
    val greens: Long = cubes.getOrElse("green", 0L)
    val blues: Long  = cubes.getOrElse("blue", 0L)

  object Hand:

    def fromString(s: String): Hand =
      Hand(s.split(',').map(_.trim).map({ case s"$n $c" => c -> n.toLong }).toMap)

  case class Game(id: Int, hands: Set[Hand]):

    def possibleWith(reds: Long, greens: Long, blues: Long): Boolean =
      hands.forall(h => h.reds <= reds && h.greens <= greens && h.blues <= blues)

    def power: Long =
      val r = hands.map(_.reds).max
      val g = hands.map(_.greens).max
      val b = hands.map(_.blues).max
      r * g * b

  object Game:

    def fromString(s: String): Game =
      s match
        case s"Game $nr: $hands" =>
          Game(nr.toInt, hands.split(';').map(Hand.fromString).toSet)
        case _ =>
          sys.error(s"unmatched $s")

  val games: Vector[Game] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Game.fromString)
      .toVector

  val start1: Long =
    System.currentTimeMillis

  val answer1: Long =
    games.filter(_.possibleWith(12, 13, 14)).map(_.id).sum

  println(s"Answer AOC 2023 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    games.map(_.power).sum

  println(s"Answer AOC 2023 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
