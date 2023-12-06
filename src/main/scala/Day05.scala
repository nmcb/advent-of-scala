import scala.annotation.tailrec
import scala.io.*

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Range(min: Long, max: Long):
    assert(min <= max)

    def intersect(that: Range): Option[Range] =
      val maxmin = min max that.min
      val minmax = max min that.max
      Option.when(maxmin <= minmax)(Range(maxmin, minmax))

    def diff(that: Range): Ranges =
      def make(min: Long, max: Long): Option[Range] = Option.when(min <= max)(Range(min, max))
      this intersect that match
        case None          => Set(this)
        case Some(overlap) => Set(make(min, overlap.min - 1), make(overlap.max + 1, max)).flatten

  object Range:
    def singleton(value: Long): Range =
      Range(value, value)

    def fromSeq(seq: Seq[Long]): Range =
      val Seq(start, length) = seq
      Range(start, start + length - 1)

  type Ranges = Set[Range]

  case class Dependency(target: Long, source: Long, length: Long):
    val sourceRange: Range = Range(source, source + length - 1)
    def mapBy(that: Range): Option[Range] =
      (that intersect sourceRange).map(r => Range(r.min - source + target, r.max - source + target))

  case class Dependencies(dependencies: Set[Dependency]):
    def mapBy(that: Range): Ranges =
      val mapped   = dependencies.flatMap(dep => dep.mapBy(that))
      val unmapped = dependencies.foldLeft(Set(that))((acc,dep) => acc.flatMap(range => range.diff(dep.sourceRange)))
      mapped ++ unmapped

  case class Input(seeds: Seq[Long], chain: Seq[Dependencies]):
    private def mapDependenciesBy(that: Range): Ranges =
      chain.foldLeft(Set(that))((rs, ms) => rs.flatMap(ms.mapBy))

    def minSeedByLocation: Long =
      seeds
        .map(Range.singleton)
        .flatMap(mapDependenciesBy)
        .map(_.min)
        .min

    def minSeedRangeByLocation: Long =
      seeds
        .grouped(2)
        .map(Range.fromSeq)
        .flatMap(mapDependenciesBy)
        .map(_.min)
        .min

  lazy val input: Input =

    def parseDependency(s: String): Dependency =
      s match
        case s"$target $source $length" => Dependency(target.toLong, source.toLong, length.toLong)

    def parseDependencies(s: String): Dependencies =
      Dependencies(s.linesIterator.drop(1).map(parseDependency).toSet)

    val lines: List[String] =
      Source
        .fromInputStream(getClass.getResourceAsStream("input05.txt"))
        .mkString
        .trim
        .split("\n\n")
        .toList

    val seeds: Seq[Long] =
      lines.head match
        case s"seeds: $seeds" => seeds.split(' ').map(_.toLong).toSeq

    val dependencies: List[Dependencies] =
      lines.tail.map(parseDependencies)

    Input(seeds, dependencies)


  val start1: Long =
    System.currentTimeMillis

  val answer1: Long =
    input.minSeedByLocation

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    input.minSeedRangeByLocation

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")