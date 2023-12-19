import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.*

object Day19 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Category = Char

  case class Part(x: Int, m: Int, a: Int, s: Int):
    def +(b: Part): Part = Part(x + b.x, m + b.m, a + b.a, s + b.s)
    def -(b: Part): Part = Part(x - b.x, m - b.m, a - b.a, s - b.s)
    def >=(b: Part): Boolean = x >= b.x && m >= b.m && a >= b.a && s >= b.s
    def <=(b: Part): Boolean = x <= b.x && m <= b.m && a <= b.a && s <= b.s
    def min(b: Part): Part = Part(math.min(x, b.x), math.min(m, b.m), math.min(a, b.a), math.min(s, b.s))
    def max(b: Part): Part = Part(math.max(x, b.x), math.max(m, b.m), math.min(a, b.a), math.min(s, b.s))

    def sum: Int = x + m + a + s


    def update(category: Category, rating: Int): Part =
      category match
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
      val intersectMin = min max that.min
      val intersectMax = max min that.max
      Option.when(intersectMin <= intersectMax)(PartRange(intersectMin, intersectMax))

    def size: Long =
      val d = max - min + Part(1, 1, 1, 1)
      d.x.toLong * d.m.toLong * d.a.toLong * d.s.toLong

  object PartRange:
    val all: PartRange =
      PartRange(Part(1, 1, 1, 1), Part(4000, 4000, 4000, 4000))

  enum Comparison:
    case Lt
    case Gt

  import Comparison.*

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


  case class Rule(category: Category, comparison: Comparison, rating: Int, ruling: Ruling):

    import PartRange.all

    def validate(pos: Part): Option[Ruling] =
      val rating =
        category match
          case 'x' => pos.x
          case 'a' => pos.a
          case 'm' => pos.m
          case 's' => pos.s

      comparison match
        case Lt if rating < this.rating => Some(ruling)
        case Gt if rating > this.rating => Some(ruling)
        case _                          => None

    import PartRange.all
    import Comparison.*

    val (trueBox, falseBox) =
      comparison match
        case Lt => (all.copy(max = all.max.update(category, rating - 1)), all.copy(min = all.min.update(category, rating)))
        case Gt => (all.copy(min = all.min.update(category, rating + 1)), all.copy(max = all.max.update(category, rating)))

    def validate(partBox: PartRange): Map[PartRange, Option[Ruling]] =
      val trueMap  = (partBox intersect trueBox).map(_ -> Option.apply(ruling)).toMap
      val falseMap = (partBox intersect falseBox).map(_ -> Option.empty[Ruling]).toMap
      trueMap ++ falseMap

  object Rule:
    def fromString(s: String): Rule =
      s match
        case s"$category<$rating:$ruling" => Rule(category.head, Comparison.Lt, rating.toInt, Ruling.fromString(ruling))
        case s"$category>$rating:$ruling" => Rule(category.head, Comparison.Gt, rating.toInt, Ruling.fromString(ruling))

  case class Workflow(rules: List[Rule], fallback: Ruling):
    def rule(pos: Part): Ruling =
      def loop(rules: List[Rule]): Ruling =
        rules match
          case Nil => fallback
          case rule :: rest =>
            rule.validate(pos) match
              case Some(ruling) => ruling
              case None         => loop(rest)
      loop(rules)

    def rule(range: PartRange): Map[PartRange, Ruling] =
      def loop(rules: List[Rule], range: PartRange): Map[PartRange, Ruling] =
        rules match
          case Nil => Map(range -> fallback)
          case rule :: newRules =>
            rule.validate(range).flatMap:
              case (partBox, Some(ruling)) => Map(partBox -> ruling)
              case (partBox, None) => loop(newRules, partBox)

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
          case Accept => true
          case Reject => false
          case Defered(workflow) => loop(workflow)
      loop("in")

    def rule(range: PartRange): Set[PartRange] =
      def loop(workflow: String, range: PartRange): Set[PartRange] =
        workflows(workflow)
          .rule(range)
          .flatMap:
            case (partBox, Accept)            => Set(partBox)
            case (_, Reject)                  => Set.empty
            case (partBox, Defered(workflow)) => loop(workflow, partBox)
          .toSet
      loop("in", range)

    lazy val validParts: List[Part] =
      parts.filter(rule)

    lazy val allAcceptedSize: Long =
      rule(PartRange.all).map(_.size).sum


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
