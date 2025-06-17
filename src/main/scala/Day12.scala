import scala.io._


object Day12 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Cave  = (String, List[String])
  type Caves = Map[String, List[String]]

  val caves: Caves =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .foldLeft[Caves](Map().withDefaultValue(Nil)): (cave, line) =>
        val Array(start,end) = line.split("-")
        cave.updated(start, end :: cave(start)).updated(end, start :: cave(end))

  def decend(caves: Caves)(traverse: Cave => Boolean): Int =

    def isSmall(name: String): Boolean =
      name.head.isLower

    def loop(current: String, path: List[String]): Int =
      caves(current).foldLeft(0): (total,next) =>
        if next == "end" then
          total + 1
        else if next == "start" || (isSmall(next) && traverse(next, path.filter(isSmall))) then
          total
        else
          total + loop(next, next :: path)

    loop("start", Nil)

  val start1  = System.currentTimeMillis
  val answer1 = decend(caves)((next, path) => path.contains(next))
  println(s"Day $day answer 1 = $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(caves: Caves): Int =
    decend(caves) { (next, path) =>
      val occurrences = (next :: path).groupBy(identity).values.map(_.length)
      occurrences.exists(_ > 2) || occurrences.count(_ == 2) > 1
    }

  val start2  = System.currentTimeMillis
  val answer2 = solve2(caves)
  println(s"Day $day answer 2 = $answer2 [${System.currentTimeMillis - start2}ms]")
