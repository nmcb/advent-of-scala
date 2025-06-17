import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val frequencies: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(s => s.toInt)
      .toList

  val start1  = System.currentTimeMillis
  val answer1 = frequencies.sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def firstFrequencyFoundTwice(frequencies: List[Int]): Int =
    @tailrec
    def go(fs: List[Int], seen: Set[Int] = Set(0), acc: Int = 0): Int =
      fs match
        case Nil                              => go(frequencies, seen, acc)
        case h :: _ if seen.contains(h + acc) => h + acc
        case h :: t                           => go(t, seen + (h + acc), h + acc)
    go(frequencies)

  val start2  = System.currentTimeMillis
  val answer2 = firstFrequencyFoundTwice(frequencies)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

