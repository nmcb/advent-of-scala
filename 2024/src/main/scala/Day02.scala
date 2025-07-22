import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

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


  private val reports: Seq[Report] =
    Source.fromResource(s"input$day.txt").getLines.map(s => Report(s.split(' ').map(_.toInt).toSeq)).toSeq

  val start1: Long = System.currentTimeMillis
  val answer1: Int = reports.count(_.isSafe)
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = reports.count(r => r.isSafe || r.isSafeWithOneLevelRemoved)
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
