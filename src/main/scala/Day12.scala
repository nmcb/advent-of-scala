import scala.annotation.tailrec
import scala.collection.*
import scala.io.*

object Day12 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Line(mask: String, lengths: List[Int]):
    def unfold: Line =
      Line(Seq.fill(5)(mask).reduce(_ ++ "?" ++ _), Seq.fill(5)(lengths).reduce(_ ++ _))

  object Line:
    def fromString(s: String): Line =
      val Array(mask, groups) = s.split(' ')
      Line(mask.trim, groups.trim.split(',').map(_.toInt).toList)

  case class Input(lines: List[Line]):

    val cache = mutable.Map.empty[(String, List[Int]), Long]

    private def solve(mask: String, lengths: List[Int]): Long =
      cache.getOrElseUpdate((mask, lengths),
        (mask, lengths) match
          case (m, Nil) if m.contains('#') => 0
          case (_, Nil)                    => 1
          case ("", _ :: _)                => 0
          case (s".$mt", ls)               => solve(mt, ls)
          case (s"#$mt", lh :: lt) if mask.length >= lh =>
            val (operational, rest) = mt.splitAt(lh - 1)
            if operational.contains('.') then
              0
            else
              rest match
                case ""      => solve("", lt)
                case s".$mt" => solve(mt, lt)
                case s"?$mt" => solve(mt, lt)
                case _ => 0
          case (s"#$mt", _ :: _)  => 0
          case (s"?$mt", lengths) => solve(s".$mt", lengths) + solve(s"#$mt", lengths)
          case (mask, lengths)    => sys.error(s"illegal state: mask=$mask, lengths=$lengths")
      )

    lazy val arrangements: Long =
      lines.map(l => solve(l.mask, l.lengths)).sum

    lazy val unfoldAll: Input =
      Input(lines.map(_.unfold))

  lazy val input: Input =
    Input(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(Line.fromString)
        .toList)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.arrangements
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = input.unfoldAll.arrangements
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
