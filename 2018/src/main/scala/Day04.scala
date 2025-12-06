import scala.io.*
import scala.util.matching.*

object Day04 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Event:
    case BeginShift(guard: Int)
    case FallAsleep
    case WakeUp

  import Event.*

  type Timestamp = String

  case class Record(timestamp: Timestamp, event: Event):
    val Timestamp: Regex = """\d{4}-\d{2}-\d{2} \d{2}:(\d{2})""".r
    def minute: Int =
      timestamp match
        case Timestamp(minute) => minute.toInt

  object Record:
    val RecordLine: Regex = """\[(.{16})\] (.*)""".r
    val GuardShift: Regex = """Guard #(\d+) begins shift""".r

    def fromLines(input: Vector[String]): Vector[Record] =
      input
        .map:
          case RecordLine(timestamp, event) =>
            Record(timestamp, event match
              case GuardShift(guard) => BeginShift(guard.toInt)
              case "falls asleep"    => FallAsleep
              case "wakes up"        => WakeUp
            )
        .sortBy(_.timestamp)

  case class Shift(guard: Int, sleep: Set[Int])

  object Shift:

    def fromRecords(records: Vector[Record], shiftStarted: Option[Shift] = None, sleepStarted: Option[Int] = None): Vector[Shift] =
      if records.isEmpty then
        shiftStarted.toVector
      else
        val record = records.head
        val rest   = records.tail
        record.event match
          case BeginShift(guard) =>
            shiftStarted.toVector ++ fromRecords(rest, Some(Shift(guard, Set.empty)))
          case FallAsleep =>
            fromRecords(rest, shiftStarted, Some(record.minute))
          case WakeUp =>
            fromRecords(rest, shiftStarted.map(shift => shift.copy(sleep = shift.sleep ++ (sleepStarted.get until record.minute).toSet)))

  def solve1(records: Vector[Record]): Int =
    val shifts          = Shift.fromRecords(Day04.records)
    val guardSleeps     = shifts.groupMapReduce(_.guard)(_.sleep.toSeq)(_ ++ _)
    val (guard, sleeps) = guardSleeps.maxBy((_, minutes) => minutes.length)
    val minute          = (0 until 60).maxBy(minute => sleeps.count(_ == minute))
    guard * minute

  lazy val records: Vector[Record] =
    Record.fromLines(Source.fromResource(s"input$day.txt").getLines.toVector)

  val start1  = System.currentTimeMillis
  lazy val answer1 = solve1(records)
  println(s"Answer AOC 2018 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def solve2(records: Vector[Record]): Int =
    val shifts              = Shift.fromRecords(records)
    val guardSleeps         = shifts.groupMapReduce(_.guard)(_.sleep.toVector)(_ ++ _)
    val guardMinuteCount    = guardSleeps.view.mapValues(_.groupMapReduce(identity)(_ => 1)(_ + _)).toMap
    val minuteMaxGuardCount = (0 until 60).map(minute => minute -> guardMinuteCount.view.mapValues(_.getOrElse(minute, 0)).maxBy((_, count) => count)).toMap

    type GuardCount      = (Int,Int)
    type GuardCountEvent = (Int,(Int,Int))

    extension (guardCount: GuardCount)
      def guard: Int = guardCount._1
      def count: Int = guardCount._2

    extension (guardCountEvent: GuardCountEvent)
      def minute: Int            = guardCountEvent._1
      def guardCount: GuardCount = guardCountEvent._2

    val result = minuteMaxGuardCount.maxBy(_.guardCount.count)
    result.guardCount.guard * result.minute

  val start2  = System.currentTimeMillis
  lazy val answer2 = solve2(records)
  println(s"Answer AOC 2018 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
