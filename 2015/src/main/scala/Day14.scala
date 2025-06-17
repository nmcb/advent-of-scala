import scala.annotation.*
import scala.io.*

object Day14 extends App:

  val day: String =
    this.getClass.getName.drop(3).init


  /** Modeling */

  case class Deer(name: String, velocity: Int, flyTime: Int, restTime: Int)

  object Deer:
    def fromString(s: String): Deer =
      s match
        case s"$name can fly $speed km/s for $fly seconds, but then must rest for $rest seconds." =>
          Deer(name, speed.toInt, fly.toInt, rest.toInt)

  sealed abstract class Count(value: Int)
  case class FC(value: Int) extends Count(value)
  case class RC(value: Int) extends Count(value)

  case class Race(state: List[(Deer,Count,Int)], duration: Int, scores: Map[Deer,Int]):

    val maxDistance: Int =
      state.map((_,_,distance) => distance).max

    val lead: List[Deer] =
      state.filter((_,_,distance) => distance == maxDistance).map((deer,_,_) => deer)

    val maxScore: Int =
      scores.values.max

    def tick: Race =
      val leg = copy(state =
        state.map((deer,count,distance) =>
          count match
            case FC(c) if c + 1 <= deer.flyTime  => (deer, FC(c + 1), distance + deer.velocity)
            case FC(_)                           => (deer, RC(1), distance)
            case RC(c) if c + 1 <= deer.restTime => (deer, RC(c + 1), distance)
            case RC(_)                           => (deer, FC(1), distance + deer.velocity)
        ))
      leg.copy(scores =
        leg.scores.map((d,s) =>
          if leg.lead.contains(d) then d -> (s + 1) else d -> s
        ))

  object Race:
    def run(deer: List[Deer], duration: Int): Race =
      val start = Race(deer.map(d => (d, FC(0), 0)), duration, deer.map(_ -> 0).toMap)
      (1 to duration).foldLeft(start)((race,_) => race.tick)

  /** Input */

  val deer: List[Deer] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Deer.fromString)
      .toList

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    Race.run(deer, 2503).maxDistance

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    Race.run(deer, 2503).maxScore

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
