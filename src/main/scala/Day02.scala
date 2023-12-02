import scala.io.*

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  enum Color:
    case Red, Green, Blue

  import Color.*

  case class Hand(cubes: Map[Color,Long]):
    def within(color: Color, inGame: Long): Boolean =
      cubes.get(color).getOrElse(0L) <= inGame

    def reds: Long =
      cubes.getOrElse(Red, 0L)

    def greens: Long =
      cubes.getOrElse(Green, 0L)

    def blues: Long =
      cubes.getOrElse(Blue, 0L)

  object Hand:
    def fromString(s: String): Hand =
      def cubesOfColor(s: String): (Color, Long) =
        s.trim match
          case s"$n red" => (Red, n.toLong)
          case s"$n green" => (Green, n.toLong)
          case s"$n blue" => (Blue, n.toLong)
          case _ => sys.error(s"unmatched $s")
      Hand(s.trim.split(',').map(_.trim).map(cubesOfColor).toMap)

  case class Game(id: Int, hands: Set[Hand]):
    def within(color: Color, count: Long): Boolean =
      hands.forall(_.within(color, count))

    def minimum: Map[Color, Long] =
      Map(
        Red   -> hands.maxBy(_.reds).reds,
        Green -> hands.maxBy(_.greens).greens,
        Blue  -> hands.maxBy(_.blues).blues
      )

    def power: Long =
      val r = minimum.getOrElse(Red, 0L)
      val g = minimum.getOrElse(Green, 0L)
      val b = minimum.getOrElse(Blue, 0L)
      r * g * b

  object Game:
    def fromString(s: String): Game =
      s match
        case s"Game $nr: $hands" =>
          Game(nr.toInt, hands.trim.split(';').map(Hand.fromString).toSet)
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

  def possibleGames(red: Long, green: Long, blue: Long): Vector[Game] =
    games
      .filter(_.within(Green, green))
      .filter(_.within(Red, red))
      .filter(_.within(Blue, blue))

  val answer1: Long =
    possibleGames(12, 13, 14).map(_.id).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    games.map(_.power).sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
