import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Directions(cycle: String):
    def next: (Char, Directions) =
      (cycle.head, Directions(cycle.tail + cycle.head))

  type Nodes = Map[String, (String, String)]

  case class Network(directions: Directions, nodes: Nodes):
    def step(from: String, direction: Char): String =
      direction match
        case 'L' => nodes.getOrElse(from, sys.error(s"no step left from: $from"))._1
        case 'R' => nodes.getOrElse(from, sys.error(s"no step left from: $from"))._2
        case _   => sys.error(s"invalid direction: $direction")

    def pathTo(exit: String => Boolean, from: String, directions: Directions, path: String = ""): String =
      if exit(from) then
        path
      else
        val (direction, next) = directions.next
        val node = step(from, direction)
        pathTo(exit, node, next, path + direction)

    def steps1: Int =
      pathTo(_ == "ZZZ", "AAA", directions).length

    def gcd(a: Long, b: Long): Long =
      if (b == 0) a.abs else gcd(b, a % b)

    def lcm(a: Long, b: Long): Long =
      (a * b).abs / gcd(a, b)

    def step2: Long =
      // note: each start node has a repeating path, ending with a node that ends with a 'Z'
      val starts = nodes.keys.filter(_.endsWith("A")).toSet
      val paths  = starts.map(from => pathTo(_.endsWith("Z"), from, directions))
      paths.map(_.length.toLong).foldLeft(1L)(lcm)


  lazy val network: Network =
    val lines: List[String] = Source.fromResource(s"input$day.txt").getLines.toList
    Network(Directions(lines.head), lines.drop(2).map { case s"$src = ($left, $right)" => src -> (left, right) }.toMap)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = network.steps1
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = network.step2
  println(s"Answer day $day part 1: ${answer2} [${System.currentTimeMillis - start2}ms]")
