import scala.io._

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Box(l: Int, h: Int, w: Int):
    def area: Int =
      2*l*w + 2*w*h + 2*h*l

    def slack: Int =
      List(l*w, w*h, h*l).min

    def wrap: Int =
      List(2*l + 2*h, 2*l + 2*w, 2*h + 2*w).min

    def volume: Int =
      l*h*w      

  val boxes: List[Box] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map{ case s"${l}x${h}x${w}" => Box(l.toInt, h.toInt, w.toInt) }
      .toList

  val answer1: Int =
    boxes.map(b => b.area + b.slack).sum
  
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    boxes.map(b => b.volume + b.wrap).sum
  
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
