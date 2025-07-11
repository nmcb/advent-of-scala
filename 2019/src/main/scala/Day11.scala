import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App:

  import cpu.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Dir:
    case N, E, S, W

    def cw: Dir =
      this match
        case N => E
        case E => S
        case S => W
        case W => N

    def ccw: Dir =
      this match
        case N => W
        case W => S
        case S => E
        case E => N

  import Dir.*

  case class Pos(x: Int, y: Int):
    infix def move(dir: Dir): Pos =
      dir match
        case N => copy(y = y - 1)
        case E => copy(x = x + 1)
        case S => copy(y = y + 1)
        case W => copy(x = x - 1)

  type Panels = Map[Pos,Long]

  object Panels:
    val empty: Panels = Map.empty.withDefaultValue(0L)

  case class Robot(cpu: CPU, pos: Pos = Pos(0,0), dir: Dir = N, panels: Panels = Panels.empty):
    @tailrec
    final def paint: Panels =
      cpu.withInput(panels(pos)).outputStates match
        case LazyList() =>
          panels
        case LazyList((_,color),(next,turn)) =>
          val rotate = if turn == 1 then dir.cw else dir.ccw
          Robot(
            cpu    = next,
            pos    = pos move rotate,
            dir    = rotate,
            panels = panels.updated(pos, color)
          ).paint

  val robot: Robot =
    val program: Mem = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)
    Robot(
      cpu    = CPU(program),
      pos    = Pos(0,0),
      dir    = N,
      panels = Panels.empty
    )

  val start1  = System.currentTimeMillis
  val answer1 = robot.paint.size
  println(s"Answer AOC 2019 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  extension (panels: Panels)
    def asString: String =
      val minX = panels.keys.map(_.x).min
      val maxX = panels.keys.map(_.x).max
      val minY = panels.keys.map(_.y).min
      val maxY = panels.keys.map(_.y).max

      val sb = StringBuffer()
      for y <- minY to maxY do
        for x <- minX to maxX do
          sb.append(if panels(Pos(x,y)) == 1 then '█' else '░')
        sb.append("\n")
      sb.toString

  val start2  = System.currentTimeMillis
  val answer2 = robot.copy(panels = Map(Pos(0,0) -> 1L).withDefaultValue(0L)).paint.asString
  println(s"Answer AOC 2019 day $day part 2:\n$answer2 [${System.currentTimeMillis - start2}ms]")
