import scala.io.Source

object Day24 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val weights: Vector[Long] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toLong)
      .toVector

  def solve(weights: Vector[Long], groups: Int): Long =
    val balance = weights.sum / groups
    Iterator
      .from(1)                          // We increment from 1 so it's the minimum
      .flatMap: configuration =>        // configuration for one group, then we
        weights                         // compute all possible weight combinations
          .combinations(configuration)  // for that configuration and filter out
          .filter(_.sum == balance)     // combinations that sum to balance assuming
          .map(_.product)               // all other groups can be balanced, finally
      .next                             // we calculate the quantum entanglement.

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve(weights, 3)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve(weights, 4)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
