import Day15.{Ingredient, day}

object Day16 extends App:

  val day: String =
    this.getClass.getName.drop(3).init


  /** Modeling */

  case class Sue(nr: Int, properties: Map[String,Int]):
    def matches1: Boolean =
      properties.forall((name, count) =>
        Found.compounds.get(name).filter(_ == count).isDefined)

    def matches2: Boolean =
      properties.forall((name, count) =>
        name match
          case "cats" | "trees" =>
            Found.compounds.get(name).filter(_ < count).isDefined
          case "pomeranians" | "goldfish" =>
            Found.compounds.get(name).filter(_ > count).isDefined
          case _ =>
            Found.compounds.get(name).filter(_ == count).isDefined)

  object Sue:
    def fromString(s: String): Sue =
      s match
        case s"Sue $nr: $n1: $c1, $n2: $c2, $n3: $c3" =>
          Sue(nr.toInt, Map(n1 -> c1.toInt, n2 -> c2.toInt, n3 -> c3.toInt))

  object Found:
    val compounds: Map[String,Int] =
      Map( "children"    -> 3
         , "cats"        -> 7
         , "samoyeds"    -> 2
         , "pomeranians" -> 3
         , "akitas"      -> 0
         , "vizslas"     -> 0
         , "goldfish"    -> 5
         , "trees"       -> 3
         , "cars"        -> 2
         , "perfumes"    -> 1
         )

  /** Input */

  val sues: List[Sue] =
    scala.io.Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Sue.fromString)
      .toList

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    val List(sue) = sues.filter(_.matches1)
    sue.nr

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    val List(sue) = sues.filter(_.matches2)
    sue.nr

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
