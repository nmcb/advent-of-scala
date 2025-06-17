import scala.io.Source

object Day25 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int, z: Int, w: Int):
    def manhattan(that: Pos): Int =
      (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs

  def solve(positions: Vector[Pos]): Int =
    positions
      .foldLeft(Set.empty[Set[Pos]]): (constellations,pos) =>
          val (near,far) = constellations.partition(_.exists(_.manhattan(pos) <= 3))
          far + (near.flatten + pos)
      .size

  val positions: Vector[Pos] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$x,$y,$z,$w" => Pos(x.toInt, y.toInt, z.toInt, w.toInt)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = solve(positions)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
