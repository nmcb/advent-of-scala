import scala.io.*

object Day03 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val start1: Long =
    System.currentTimeMillis

  val commands: List[Char] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .toList

  case class Area(start: Area.Loc = Area.Loc.init, deliveries: Area.Deliveries = Area.Deliveries.init):

    private def moveToAndDeliver(loc: Area.Loc): Area =
      copy(start = loc, deliveries = deliveries.add(loc))

    infix def next(cmd: Area.Command): Area =
      cmd match
        case '>' => moveToAndDeliver(start.copy(x = start.x + 1))
        case '<' => moveToAndDeliver(start.copy(x = start.x - 1))
        case '^' => moveToAndDeliver(start.copy(y = start.y + 1))
        case 'v' => moveToAndDeliver(start.copy(y = start.y - 1))

  object Area:

    case class Loc(x: Int, y: Int)

    object Loc:
      def init: Loc =
        Loc(0, 0)

    type Deliveries = Map[Loc,Int]

    object Deliveries:

      def empty: Deliveries =
        Map.empty

      def init: Deliveries =
        empty.add(Loc.init)

    extension (d: Deliveries) def add(loc: Loc): Deliveries =
      d.updatedWith(loc)(_.orElse(Some(0)).map(_ + 1))

    type Command = Char

    def init: Area =
      Area(start = Area.Loc.init, deliveries = Area.Deliveries.init)

  val answer1: Int =
    commands
      .foldLeft(Area.init)(_ next _)
      .deliveries
      .values
      .size

  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val (robot, santa) =
    commands
      .zipWithIndex
      .foldLeft((Area.init, Area.init)) { case ((robot, santa), (command, index)) =>
        if (index % 2 != 0) (robot next command, santa) else (robot, santa next command)
      }

  val answer2: Int =
    (robot.deliveries ++ santa.deliveries).values.size
  
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
