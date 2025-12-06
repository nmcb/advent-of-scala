import scala.io.Source

object Day23 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int, z: Int):
    infix def manhattan(that: Pos): Int = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

  object Pos:
    val origin: Pos = Pos(0,0,0)

  case class Bot(pos: Pos, radius: Int):
    infix def hasOverlappingRadiusWith(that: Bot): Boolean =
      pos.manhattan(that.pos) <= radius + that.radius

  val bots: Set[Bot] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .collect:
        case s"pos=<$x,$y,$z>, r=$r" => Bot(Pos(x.toInt, y.toInt, z.toInt), r.toInt)
      .toSet

  def solve1(bots: Set[Bot]): Int =
    val strongest = bots.maxBy(_.radius)
    val reachable = bots.count(_.pos.manhattan(strongest.pos) <= strongest.radius)
    reachable

  val start1  = System.currentTimeMillis
  lazy val answer1 = solve1(bots)
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  /** Computes the maximum clique, i.e. the largest group of bots with overlapping radiuses. */
  def bronKerbosch(overlappingRadiusesOf: Map[Bot,Set[Bot]]): Set[Bot] =
    var maximum: Set[Bot] = Set.empty

    def loop(r: Set[Bot], p: Set[Bot], x: Set[Bot]): Unit =
      if p.isEmpty && x.isEmpty then
        if r.size > maximum.size then maximum = r
      else
        // Technically this implements Tomita's optimisation which recurses with a pivot node
        // chosen to contain the maximum number of edges. This variant of the Bron-Kerbosch
        // algorithm performs up to twice as fast on sparse, and similar on dense graphs.
        val u = (p union x).maxBy(overlappingRadiusesOf(_).size)
        var bp = p
        var bx = x
        for
          v <- p diff overlappingRadiusesOf(u)
        yield
          loop(r + v, p intersect overlappingRadiusesOf(v), x intersect overlappingRadiusesOf(v))
          bp -= v
          bx += v

    loop(Set.empty, overlappingRadiusesOf.keySet, Set.empty)
    maximum

  def solve2(bots: Set[Bot]): Int =
    val overlappingRadiusesOf: Map[Bot,Set[Bot]] =
      bots
        .map: bot =>
          bot -> bots.filter: other =>
            bot != other && bot.hasOverlappingRadiusWith(other)
        .toMap

    bronKerbosch(overlappingRadiusesOf).map(bot => bot.pos.manhattan(Pos.origin) - bot.radius).max

  val start2  = System.currentTimeMillis
  lazy val answer2 = solve2(bots)
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
