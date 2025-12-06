import scala.io.Source

object Day13 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Layers = Map[Int,Int]

  val layers: Layers =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$depth: $range" => depth.toInt -> range.toInt
      .toMap

  extension (layers: Layers)

    def rangesCaught(delay: Int): Set[Int] =
      def caught(range: Int, time: Int): Boolean = time % (2 * (range - 1)) == 0
      layers.filter((depth, range) => caught(range, delay + depth)).keySet

    def tripSeverity: Int =
      rangesCaught(delay = 0).map(depth => depth * layers(depth)).sum

    def uncaughtDelay: Int =
      Iterator.from(0).find(delay => layers.rangesCaught(delay).isEmpty).get


  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = layers.tripSeverity
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = layers.uncaughtDelay
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
