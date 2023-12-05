import scala.annotation.tailrec
import scala.io.*

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Range(from: Long, to: Long):
    assert(from <= to)

    def merge(that: Range): Option[Range] =
      val min = from max that.from
      val max = to   min that.to
      Option.when(min <= max)(Range(min, max))

    def split(that: Range): Ranges =
      def make(min: Long, max: Long): Option[Range] = Option.when(min <= max)(Range(min, max))
      this merge that match
        case None          => Set(this)
        case Some(overlap) => Set(make(from, overlap.from - 1), make(overlap.to + 1, to)).flatten

  object Range:
    def singleton(value: Long): Range =
      Range(value, value)

    def fromSeq(seq: Seq[Long]): Range =
      val Seq(start, length) = seq
      Range(start, start + length - 1)

  type Ranges = Set[Range]

  case class RangeEntry(target: Long, source: Long, length: Long):
    val range: Range = Range(source, source + length - 1)
    def mapToRange(from: Range): Option[Range] =
      (from merge range).map(r => Range(r.from - source + target, r.to - source + target))

  case class RangeMap(entries: Seq[RangeEntry]):
    def mapToRanges(from: Range): Ranges =
      val mapped   = entries.flatMap(_.mapToRange(from)).toSet
      val unmapped = entries.foldLeft(Set(from))((rs,re) => rs.flatMap(_.split(re.range)))
      mapped ++ unmapped

  case class Input(seeds: Seq[Long], maps: Seq[RangeMap]):
    def mapToRanges(from: Range): Ranges =
      maps.foldLeft(Set(from))((rs, ms) => rs.flatMap(ms.mapToRanges))

    def minSeedByLocation: Long =
      seeds
        .map(Range.singleton)
        .flatMap(mapToRanges)
        .map(_.from)
        .min

    def minSeedRangeByLocation: Long =
      seeds
        .grouped(2)
        .map(Range.fromSeq)
        .flatMap(mapToRanges)
        .map(_.from)
        .min

  lazy val input: Input =

    def parseRangeEntry(s: String): RangeEntry =
      s match
        case s"$target $source $length" => RangeEntry(target.toLong, source.toLong, length.toLong)

    def parseRangeMap(s: String): RangeMap =
      RangeMap(s.linesIterator.drop(1).map(parseRangeEntry).toSeq)

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

    val rangeMaps: List[RangeMap] =
      lines.tail.map(parseRangeMap)

    Input(seeds, rangeMaps)

  
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