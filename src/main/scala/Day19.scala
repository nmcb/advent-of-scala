import scala.annotation.tailrec
import scala.io.Source

object Day19 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Part(x: Long, m: Long, a: Long, s: Long)

  enum Rule:
    case Accepted
    case Rejected
    case ToOtherWorkflow(workflow: String)
    case Comparison(select1: Part => Boolean, select2: Long => Boolean, workflow: String, selector: Char)

  import Rule.*

  case class Range(from: Long, to: Long)

  case class State(x: Range, m: Range, a: Range, s: Range, workflow: String)

  type Workflows = Map[String, List[Rule]]

  def parse(lines: List[String]): (List[Part], Workflows) =
    def parseWorkflow(s: String): (String, List[Rule]) = s match {
      case s"$workflowName{$rulesString}" =>
        val rules = rulesString
          .split(',')
          .map { s =>
            if (s.contains('<') || s.contains('>')) {
              val selector: (Part => Long) =
                if (s.startsWith("x")) _.x
                else if (s.startsWith("m")) _.m
                else if (s.startsWith("a")) _.a
                else _.s
              val valueStr = s.drop(2).takeWhile(_.isDigit)
              val value    = valueStr.toLong
              val workflow = s.drop(3 + valueStr.length)
              s(1) match {
                case '<' => Comparison(selector(_) < value, _ < value, workflow, s.head)
                case '>' => Comparison(selector(_) > value, _ > value, workflow, s.head)
              }
            } else if (s == "R") {
              Rejected
            } else if (s == "A") {
              Accepted
            } else {
              ToOtherWorkflow(s)
            }
          }
          .toList

        workflowName -> rules
    }

    val workflows =
      lines.takeWhile(_.nonEmpty).map(parseWorkflow).toMap + ("A" -> List(Accepted)) + ("R" -> List(
        Rejected
      ))

    val parts = lines.drop(workflows.size + 1 - 2).map { case s"{x=$x,m=$m,a=$a,s=$s}" =>
      Part(x.toLong, m.toLong, a.toLong, s.toLong)
    }

    (parts, workflows)

  val lines: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines()
      .toList

  val (parts, workflows) = parse(lines)


  def solve1(): Long =
    def loop(part: Part, workflow: String, workflows: Workflows): Rule =
      val rules = workflows(workflow)
      val matchedRule =
        rules
          .find:
            case Accepted                            => true
            case Rejected                            => true
            case ToOtherWorkflow(workflow)           => true
            case Comparison(select1, _, workflow, _) => select1(part)
          .getOrElse(sys.error(s"no rule matched: $workflow"))

      matchedRule match
        case Accepted => Accepted
        case Rejected => Rejected
        case ToOtherWorkflow(workflow) => loop(part, workflow, workflows)
        case Comparison(_, _, workflow, _) => loop(part, workflow, workflows)


    parts
      .filter(part => loop(part, "in", workflows) == Accepted)
      .map(p => p.x + p.m + p.a + p.s)
      .sum

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve1()
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def solve2(): Long =

    def loop(searches: List[State], workflows: Workflows, found: List[State]): List[State] =
      if searches.isEmpty then
        found
      else
        val search = searches.head
        val rules = workflows(search.workflow)

        def selector(selectorChar: Char): State => Range =
          if selectorChar == 'x' then _.x
          else if selectorChar == 'm' then _.m
          else if selectorChar == 'a' then _.a
          else _.s

        val matched =
          rules.find:
            case Accepted => true
            case Rejected => true
            case ToOtherWorkflow(workflow) => true
            case Comparison(_, select2, workflow, char) =>
              val min = selector(char)(search).from
              val max = selector(char)(search).to
              (min to max).exists(select2)
          .getOrElse(sys.error(s"no rule matched"))

        matched match
          case Accepted => loop(searches.tail, workflows, search +: found)
          case Rejected => loop(searches.tail, workflows, found)
          case ToOtherWorkflow(workflow) => loop(search.copy(workflow = workflow) +: searches.tail, workflows, found)
          case Comparison(_, select, workflow, char) =>
            val min = selector(char)(search).from
            val max = selector(char)(search).to
            val included = (min to max).map(select)
            val split = (1 until included.size).find(i => included(i - 1) != included(i))

            split match
              case None => loop(search.copy(workflow = workflow) +: searches.tail, workflows, found)
              case Some(at) =>

                def make(range: Range): List[Range] =
                  List(Range(range.from, range.from + at - 1), Range(range.from + at, range.to))

                val splits =
                  if      char == 'x' then make(search.x).map(r => search.copy(x = r))
                  else if char == 'm' then make(search.m).map(r => search.copy(m = r))
                  else if char == 'a' then make(search.a).map(r => search.copy(a = r))
                  else                     make(search.s).map(r => search.copy(s = r))

                loop(splits ++ searches.tail, workflows, found)

    val start = State(Range(1, 4000), Range(1, 4000), Range(1, 4000), Range(1, 4000), "in")
    val ranges = loop(List(start), workflows, List.empty)

    ranges
      .map: r =>
        val x = r.x.to - r.x.from + 1
        val m = r.m.to - r.m.from + 1
        val a = r.a.to - r.a.from + 1
        val s = r.s.to - r.s.from + 1
        x * m * a * s
      .sum

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve2()
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")