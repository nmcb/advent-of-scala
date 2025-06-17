import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  /** Modeling */

  case class Task(inst: String, x0: Int, y0: Int, x1: Int, y1: Int)

  object Task:
    def apply(cmd: String, p0x: String, p0y: String, p1x: String, p1y: String): Task =
      Task(cmd, p0x.toInt, p0y.toInt, p1x.toInt, p1y.toInt)

  abstract class Lights[A](tasks: List[Task]):
    val zero:   A
    val toggle: A => A
    val on:     A => A
    val off:    A => A

    def instruction(a: A, task: Task): A =
      task.inst match
        case "toggle" => toggle(a)
        case "on"     => on(a)
        case "off"    => off(a)

    def affects(x: Int, y: Int)(t: Task): Boolean =
      x >= t.x0 && x <= t.x1 && y >= t.y0 && y <= t.y1

    def exec(x: Int, y: Int): A =
      tasks.filter(affects(x, y)).foldLeft(zero)(instruction)

    def run: List[A] =
      List.tabulate(1000, 1000)(exec).flatten

  val input: List[Task] =
    def parser(s: String): Task =
      s match
        case s"toggle $p0x,$p0y through $p1x,$p1y"   => Task("toggle", p0x, p0y, p1x, p1y)
        case s"turn on $p0x,$p0y through $p1x,$p1y"  => Task("on"    , p0x, p0y, p1x, p1y)
        case s"turn off $p0x,$p0y through $p1x,$p1y" => Task("off"   , p0x, p0y, p1x, p1y)

    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parser)
      .toList

  /** Part 1 */

  object BooleanLights extends Lights[Boolean](input):
    val zero   = false
    val toggle = v => !v
    val on     = _ => true
    val off    = _ => false

  val start1: Long = System.currentTimeMillis
  val answer1: Int = BooleanLights.run.count(identity)

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  object IntLights extends Lights[Int](input):
    val zero   = 0
    val toggle = v => v + 2
    val on     = v => v + 1
    val off    = v => if v <= 0 then 0 else v - 1

  val start2: Long = System.currentTimeMillis
  val answer2: Int = IntLights.run.sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
