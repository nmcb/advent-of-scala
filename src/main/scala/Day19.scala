import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.*

object Day19 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Part(x: Int, m: Int, a: Int, s: Int):
    def +(that: Part): Part = Part(x + that.x, m + that.m, a + that.a, s + that.s)
    def -(that: Part): Part = Part(x - that.x, m - that.m, a - that.a, s - that.s)
    def >=(that: Part): Boolean = x >= that.x && m >= that.m && a >= that.a && s >= that.s
    def <=(that: Part): Boolean = x <= that.x && m <= that.m && a <= that.a && s <= that.s
    def min(that: Part): Part = Part(x min that.x, m min that.m, a min that.a, s min that.s)
    def max(that: Part): Part = Part(x max that.x, m max that.m, a max that.a, s max that.s)

    def sum: Int = x + m + a + s

    def update(char: Char, rating: Int): Part =
      char match
        case 'x' => copy(x = rating)
        case 'm' => copy(m = rating)
        case 'a' => copy(a = rating)
        case 's' => copy(s = rating)

  object Part:

    def empty: Part =
      Part(0, 0, 0, 0)

    def fromString(s: String): Part =
      s match
        case s"{$ratings}" =>
          ratings.split(',').foldLeft(Part.empty):
            case (p, s"x=$rating") => p.copy(x = rating.toInt)
            case (p, s"m=$rating") => p.copy(m = rating.toInt)
            case (p, s"a=$rating") => p.copy(a = rating.toInt)
            case (p, s"s=$rating") => p.copy(s = rating.toInt)


  case class PartRange(min: Part, max: Part):

    def intersect(that: PartRange): Option[PartRange] =
      val maxmin = min max that.min
      val minmax = max min that.max
      Option.when(maxmin <= minmax)(PartRange(maxmin, minmax))

    def size: Long =
      val delta = max - min + Part(1, 1, 1, 1)
      delta.x.toLong * delta.m.toLong * delta.a.toLong * delta.s.toLong

  object PartRange:
    val count: PartRange =
      PartRange(Part(1, 1, 1, 1), Part(4000, 4000, 4000, 4000))

  enum Compare:
    case LT
    case GT

  import Compare.*

  enum Ruling:
    case Accept
    case Reject
    case Defered(workflow: String)

  object Ruling:
    def fromString(s: String): Ruling =
      s match
        case "A"  => Ruling.Accept
        case "R"  => Ruling.Reject
        case name => Ruling.Defered(name)

  import Ruling.*

  case class Rule(char: Char, compare: Compare, rating: Int, ruling: Ruling):
    def rule(part: Part): Option[Ruling] =
      val r =
        char match
          case 'x' => part.x
          case 'a' => part.a
          case 'm' => part.m
          case 's' => part.s

      compare match
        case LT if r < rating => Some(ruling)
        case GT if r > rating => Some(ruling)
        case _                => None

    import PartRange.count
    import Compare.*

    val (rangeT, rangeF) =
      compare match
        case LT => (count.copy(max = count.max.update(char, rating - 1)), count.copy(min = count.min.update(char, rating)))
        case GT => (count.copy(min = count.min.update(char, rating + 1)), count.copy(max = count.max.update(char, rating)))

    def rule(range: PartRange): Map[PartRange, Option[Ruling]] =
      val mapT = (range intersect rangeT).map(_ -> Option.apply(ruling)).toMap
      val mapF = (range intersect rangeF).map(_ -> Option.empty[Ruling]).toMap
      mapT ++ mapF

  object Rule:
    def fromString(s: String): Rule =
      s match
        case s"$category<$rating:$ruling" => Rule(category.head, LT, rating.toInt, Ruling.fromString(ruling))
        case s"$category>$rating:$ruling" => Rule(category.head, GT, rating.toInt, Ruling.fromString(ruling))

  case class Workflow(rules: List[Rule], otherwise: Ruling):
    def rule(pos: Part): Ruling =
      def loop(rules: List[Rule]): Ruling =
        rules match
          case Nil => otherwise
          case rule :: rest =>
            rule.rule(pos) match
              case Some(ruling) => ruling
              case None         => loop(rest)
      loop(rules)

    def rule(range: PartRange): Map[PartRange, Ruling] =
      def loop(rules: List[Rule], range: PartRange): Map[PartRange, Ruling] =
        rules match
          case Nil => Map(range -> otherwise)
          case rule :: rest =>
            rule.rule(range).flatMap:
              case (range, Some(ruling)) => Map(range -> ruling)
              case (range, None)         => loop(rest, range)

      loop(rules, range)

  object Workflow:
    def fromString(s: String): (String, Workflow) =
      s match
        case s"$name{$specification}" =>
          val components = specification.split(',')
          val rules      = components.init.map(Rule.fromString).toList
          val otherwise  = Ruling.fromString(components.last)
          name -> Workflow(rules, otherwise)

  case class Input(workflows: Map[String, Workflow], parts: List[Part]):
    def rule(pos: Part): Boolean =
      def loop(workflow: String): Boolean =
        workflows(workflow).rule(pos) match
          case Accept            => true
          case Reject            => false
          case Defered(workflow) => loop(workflow)
      loop("in")

    def rule(range: PartRange): Set[PartRange] =
      def loop(workflow: String, range: PartRange): Set[PartRange] =
        workflows(workflow)
          .rule(range)
          .flatMap:
            case (range, Accept)            => Set(range)
            case (_, Reject)                => Set.empty
            case (range, Defered(workflow)) => loop(workflow, range)
          .toSet
      loop("in", range)

    lazy val validParts: List[Part] =
      parts.filter(rule)

    lazy val allAcceptedSize: Long =
      rule(PartRange.count).map(_.size).sum


  object Input:
    def fromString(s: String): Input =
      val Array(ws, ps) = s.split("\n\n")
      val workflows = ws.split("\n").map(Workflow.fromString)
      val parts     = ps.split("\n").map(Part.fromString)
      Input(workflows.toMap, parts.toList)


  lazy val input: Input =
    Input.fromString(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.validParts.map(_.sum).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = input.allAcceptedSize
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
