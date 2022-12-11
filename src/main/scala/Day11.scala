import scala.io.Source
import scala.util.Try

object Day11 extends App:

  val day: String = this.getClass.getName.drop(3).init

  lazy val monkeys: List[Monkey] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .filterNot(_.isBlank)
      .grouped(6)
      .map(_.toList)
      .map(Monkey.fromStrings)
      .toList

  val leastCommonMultiple: Long =
    monkeys.map(_.div).product

  object Monkey:
    def fromStrings(ss: List[String]): Monkey =
      def parseMonkey(s: String): Int =
        s match
          case s"Monkey $nr:" => nr.toInt
      def parseItems(s: String): List[Long] =
        s match
          case s"  Starting items: $is" => is.split(',').map(n => n.trim.toLong).toList
      def parseOperation(s: String): (String,String) =
        s match
          case s"  Operation: new = old $op $r" => (op,r)
      def parseTest(s: String): Long =
        s match
          case s"  Test: divisible by $d" => d.toLong
      def parseTrue(s: String): Int =
        s match
          case s"    If true: throw to monkey $m" => m.toInt
      def parseFalse(s: String): Int =
        s match
          case s"    If false: throw to monkey $m" => m.toInt

      val nr    = parseMonkey(ss(0))
      val is    = parseItems(ss(1))
      val (o,r) = parseOperation(ss(2))
      val test  = parseTest(ss(3))
      val tt    = parseTrue(ss(4))
      val tf    = parseFalse(ss(5))

      Monkey(nr, is, o, Try(r.toLong).toOption, test, tt, tf)


  case class Monkey(nr: Int, items: List[Long], op: String, rhs: Option[Long], div: Long, throwToTrue: Int, throwToFalse: Int, count: Long = 0, part2: Boolean = false):
    def operation(i: Long): Long =
      op match
        case "+" => i + rhs.getOrElse(i)
        case "*" => i * rhs.getOrElse(i)

    private def inspect(i: Long): (Long,Int) =
      val w1 = operation(i)
      val w2 = if !part2 then w1 / 3 else w1 % leastCommonMultiple
      val nr = if w2 % div == 0 then throwToTrue else throwToFalse
      (w2,nr)

    def inspect: List[(Long,Int)] =
      items.map(inspect)

  def round(ms: List[Monkey]): List[Monkey] =
    def loop(ms: List[Monkey], acc: List[Monkey]): List[Monkey] =
      ms match
        case Nil => acc
        case m :: t =>
          val us  = acc(m.nr).inspect
          val ums = us.foldLeft(acc) { case (s, (i, nr)) =>
            s.updated(nr, s(nr).copy(items = s(nr).items :+ i))
          }
          val nms = ums.updated(m.nr, ums(m.nr).copy(items = List.empty, count = m.count + us.size))
          loop(t, nms)
    loop(ms, ms)

  val start1: Long =
    System.currentTimeMillis

  val answer1: Long =
    (1 to 20)
      .foldLeft(monkeys)((s,_) => round(s))
      .map(_.count)
      .sorted
      .takeRight(2)
      .product

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val play2 =
    (1 to 10000)
      .foldLeft(monkeys.map(_.copy(part2 = true)))((s,_) => round(s))

  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    (1 to 10000)
      .foldLeft(monkeys.map(_.copy(part2 = true)))((s,_) => round(s))
      .map(_.count)
      .sorted
      .takeRight(2)
      .product

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
