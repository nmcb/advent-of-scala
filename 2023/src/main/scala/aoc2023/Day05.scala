package aoc2023

import nmcb.*

object Day05 extends AoC:

  case class Range(min: Long, max: Long):
    assert(min <= max)

    import Range.optional

    infix def intersect(that: Range): Option[Range] =
      val maxmin = min max that.min
      val minmax = max min that.max
      optional(min = maxmin, max = minmax)

    infix def diff(that: Range): Set[Range] =
      this intersect that match
        case None          => Set(this)
        case Some(overlap) => Set(optional(min, overlap.min - 1), optional(overlap.max + 1, max)).flatten

  object Range:
    def singleton(value: Long): Range =
      Range(value, value)

    def fromSeq(seq: Seq[Long]): Range =
      val Seq(start, length) = seq
      Range(start, start + length - 1)

    def optional(min: Long, max: Long): Option[Range] =
      Option.when(min <= max)(Range(min, max))

  case class Dependency(target: Long, source: Long, length: Long):
    val sourceRange: Range = Range(source, source + length - 1)
    infix def mapBy(that: Range): Option[Range] =
      (that intersect sourceRange).map(r => Range(r.min - source + target, r.max - source + target))

  case class Dependencies(dependencies: Set[Dependency]):
    infix def mapBy(that: Range): Set[Range] =
      val mapped   = dependencies.flatMap(_ mapBy that)
      val unmapped = dependencies.foldLeft(Set(that))((acc,dep) => acc.flatMap(_ diff dep.sourceRange))
      mapped ++ unmapped

  case class Puzzle(seeds: Seq[Long], chain: Seq[Dependencies]):
    
    private def mapDependenciesBy(that: Range): Set[Range] =
      chain.foldLeft(Set(that))((rs,ms) => rs.flatMap(ms.mapBy))

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

  lazy val puzzle: Puzzle =

    def parseDependency(s: String): Dependency =
      s match
        case s"$target $source $length" => Dependency(target.toLong, source.toLong, length.toLong)

    def parseDependencies(s: String): Dependencies =
      Dependencies(s.linesIterator.drop(1).map(parseDependency).toSet)

    val lines: Seq[String] =
      input
        .split("\n\n")
        .toSeq

    val seeds: Seq[Long] =
      lines.head match
        case s"seeds: $seeds" => seeds.split(' ').map(_.toLong).toSeq

    val dependencies: Seq[Dependencies] =
      lines.tail.map(parseDependencies)

    Puzzle(seeds, dependencies)


  lazy val answer1: Long = puzzle.minSeedByLocation
  lazy val answer2: Long = puzzle.minSeedRangeByLocation
