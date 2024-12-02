import scala.io.*

object Day02 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Report(levels: List[Int]):

    private lazy val differences: List[Int] =
      levels
        .sliding(2)
        .map:
          case List(l, r) => r - l
          case d => sys.error(s"boom: $d")
        .toList

    private lazy val isIncreasing: Boolean =
      differences.forall(d => d > 0)

    private lazy val isDecreasing: Boolean =
      differences.forall(d => d < 0)

    private lazy val isBounded: Boolean =
      differences.forall(d => d.abs >= 1 && d.abs <= 3)

    def isSafe: Boolean =
      (isIncreasing || isDecreasing) && isBounded

    private lazy val withOneLevelRemoved: List[Report] =
      levels
        .indices
        .map: i =>
          Report(levels.take(i) ++ levels.drop(i + 1))
        .toList

    def isSafeWithOneLevelRemoved: Boolean =
      withOneLevelRemoved.count(_.isSafe) > 0


  val reports: List[Report] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map: s =>
        Report(s.trim.split(' ').map(_.trim.toInt).toList)
      .toList

  val start1: Long = System.currentTimeMillis
  val answer1: Int = reports.count(_.isSafe)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = reports.count(r => r.isSafe || r.isSafeWithOneLevelRemoved)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
