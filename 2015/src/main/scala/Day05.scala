import scala.io.*

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val strings: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val answer1: Int =
    val valid = for {
      str <- strings
      if str.count("aeiou".toList.contains) >= 3
      if str.sliding(2).map(_.toArray).count(cs => cs(0) == cs(1)) >= 1
      if str.sliding(2).count(cs => cs == "ab" || cs == "cd" || cs == "pq" || cs == "xy") == 0
    } yield str
    valid.size

  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    def containsTwoOrMoreNonOverlappingPair(str: String): Boolean =
      def hasNonOverlappingPair(pair: String, todo: String, foundFirst: Boolean = false): Boolean =
        val index = todo.indexOf(pair)
        if      (index != -1 && !foundFirst) hasNonOverlappingPair(pair, todo.drop(index + 2), true)
        else if (index != -1 &&  foundFirst) true
        else                                 false

      (str.sliding(2).distinct).exists(pair => hasNonOverlappingPair(pair, str)) 

    def containsTwoSameCharsOneCharApart(str: String): Boolean =
      def hasTwoSameCharsOneCharApart(todo: List[Char]): Boolean =
        todo match
          case c0 ::  _ :: c2 :: tail if c0 == c2 => true
          case c0 :: c1 :: c2 :: tail             => hasTwoSameCharsOneCharApart(c1 :: c2 :: tail)
          case _                                  => false
      hasTwoSameCharsOneCharApart(str.toList)

    val filter = for {
      str <- strings
      if containsTwoOrMoreNonOverlappingPair(str)
      if containsTwoSameCharsOneCharApart(str)
    } yield str
    filter.size
  
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
