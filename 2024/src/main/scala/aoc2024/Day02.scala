package aoc2024

import nmcb.*

object Day02 extends AoC:

  private case class Report(levels: Seq[Int]):

    private lazy val differences: Seq[Int] =
      levels
        .sliding(2)
        .map:
          case Seq(l, r) => r - l
          case d => sys.error(s"boom: $d")
        .toSeq

    private lazy val isIncreasing: Boolean =
      differences.forall(d => d >= 0)

    private lazy val isDecreasing: Boolean =
      differences.forall(d => d <= 0)

    private lazy val isBounded: Boolean =
      differences.forall(d => d.abs >= 1 && d.abs <= 3)

    private lazy val withOneLevelRemoved: Seq[Report] =
      levels.indices.map(i => Report(levels.take(i) ++ levels.drop(i + 1)))

    lazy val isSafe: Boolean =
      (isIncreasing || isDecreasing) && isBounded

    lazy val isSafeWithOneLevelRemoved: Boolean =
      withOneLevelRemoved.count(_.isSafe) > 0


  private val reports: Seq[Report] = lines.map(s => Report(s.split(' ').map(_.toInt).toSeq))

  lazy val answer1: Int = reports.count(_.isSafe)
  lazy val answer2: Int = reports.count(r => r.isSafe || r.isSafeWithOneLevelRemoved)
