import scala.annotation.*
import scala.io.*

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Dep(sourceT: String, targetT: String, maps: Vector[(Long,Long,Long)]):
    def sourceToTarget(source: Long): Long =
      maps.find((from, _, length) => source >= from && source < from + length) match
        case Some((from, to, _)) =>
          val delta = source - from
          to + delta
        case None =>
          source

    def targetToSource(target: Long): Long =
      maps.find((_, to, length) => target >= to && target < to + length) match
        case Some((from, to, _)) =>
          val delta = target - to
          from + delta
        case None =>
          target

  val (seeds: Vector[Long], deps: Vector[Dep]) =
    val lines: List[String] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .filter(_.nonEmpty)
        .toList

    def parse(todo: List[String], seeds: List[Long] = List.empty, deps: List[Dep] = List.empty): (Vector[Long], Vector[Dep]) =
      todo match
        case Nil => (seeds.toVector, deps.toVector)
        case s"seeds: $nums" :: rest =>
          parse(rest, nums.split(' ').map(_.toLong).toList)
        case s"$source-to-$target map:" :: rest =>
          val dep: Dep =
            Dep(source, target,
              rest
                .takeWhile(l => !l.endsWith("map:"))
                .foldLeft(Vector.empty[(Long,Long,Long)])((m,d) =>
                  val List(targetFrom, sourceFrom, length) = d.split(' ').map(_.toLong).toList
                  m :+ (sourceFrom, targetFrom, length)
                )
            )
          val next: List[String] = rest.dropWhile(l => !l.endsWith("map:"))
          parse(next, seeds, deps :+ dep)
        case _ =>
          sys.error("unmatched")

    parse(lines)

  val seedToSoil: Dep = deps.filter(d => d.sourceT == "seed" && d.targetT == "soil").head
  val soilToFertilizer: Dep = deps.filter(d => d.sourceT == "soil" && d.targetT == "fertilizer").head
  val fertilizerToWater: Dep = deps.filter(d => d.sourceT == "fertilizer" && d.targetT == "water").head
  val waterToLight: Dep = deps.filter(d => d.sourceT == "water" && d.targetT == "light").head
  val lightToTemperature: Dep = deps.filter(d => d.sourceT == "light" && d.targetT == "temperature").head
  val temperatureToHumidity: Dep = deps.filter(d => d.sourceT == "temperature" && d.targetT == "humidity").head
  val humidityToLocation: Dep = deps.filter(d => d.sourceT == "humidity" && d.targetT == "location").head

  def seedToLocation(s: Long): Long =
    Vector(seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation)
      .foldLeft(s)((id,dep) => dep.sourceToTarget(id))

  val start1: Long =
    System.currentTimeMillis

  val answer1: Long =
    seeds.map(seedToLocation).min

//  assert(answer1 == 51580674)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def locationToSeed(s: Long): Long =
    Vector(seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation)
      .foldRight(s)((dep,id) => dep.targetToSource(id))

  val start2: Long =
    System.currentTimeMillis

  val seedRanges: List[(Long,Long)] =
    seeds.grouped(2).map(l => (l(0), l(1))).toList

  def hasSeed(seed: Long): Boolean =
    seedRanges.exists((from, length) => seed >= from && seed < from + length)

  lazy val answer2: Long =
    def solve(todo: List[(Long,Long)], min: Long = Long.MaxValue): Long =
      def loop(from: Long, to: Long, min: Long = Long.MaxValue): Long =
        if from >= to then
          min
        else
          val loc = seedToLocation(from)
          if loc < min then
            loop(from + 1, to, loc)
          else
            loop(from + 1, to, min)

      todo match
        case Nil =>
          min
        case (from,length) :: rest =>
          println(s"from=$from [$length]")
          val loc = loop(from, from + length)
          if loc <= min then solve(rest, loc) else solve(rest, min)

    solve(seedRanges)


  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
