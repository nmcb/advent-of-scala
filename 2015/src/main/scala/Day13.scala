import scala.annotation.*
import scala.io.*

object Day13 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString


  /** Modeling */

  type Preferences = Map[String,Map[String,Int]]

  val preferences: Preferences  =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map {
        case s"$name would gain $value happiness units by sitting next to $neighbour." =>
          (name, neighbour, value.toInt)
        case s"$name would lose $value happiness units by sitting next to $neighbour." =>
          (name, neighbour, -value.toInt)
      }
      .foldLeft[Preferences](Map.empty) {
        case (ps, (name, neighbour, gain)) =>
          val happiness = neighbour -> gain
          ps.updatedWith(name)(_.map(_ + happiness).orElse(Some(Map(happiness))))
      }

  val names: List[String] =
    preferences.keys.toList

  case class Table(setting: List[String], prefs: Preferences):

    def boom = sys.error("boom!")

    val neighbours: Map[String,List[String]] =
      val n1 = List(setting.init.last, setting.last, setting.head)
      val n2 = List(setting.last, setting.head, setting.tail.head)
      (n1 :: n2 :: setting.sliding(3).toList)
        .map {
          case List(nl, name, nr) => name -> List(nl, nr)
          case _ => boom
        }
        .toMap

    def happiness(name: String): Int =
      val List(n1, n2) = neighbours(name)
      val h1 = prefs(name)(n1)
      val h2 = prefs(name)(n2)
      h1 + h2

    def totalHappiness: Int =
      setting.foldLeft(0)((h,n) => h + happiness(n))


  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    names
      .permutations
      .map(setting => Table(setting, preferences).totalHappiness)
      .max

  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val name: String =
    "Marco"

  val names2: List[String] =
    name :: names

  val preferences2: Preferences =
    val zero = name -> preferences.keys.map(n => n -> 0).toMap
    preferences.map((n,p) => n -> (p + (name -> 0))) + zero

  val answer2: Int =
    names2
      .permutations
      .map(setting => Table(setting, preferences2).totalHappiness)
      .max

  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
