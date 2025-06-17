import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Pattern  = String
  type PotIndex = Int
  type Rules    = Set[Pattern]
  type Plants   = Set[PotIndex]

  val (rules: Rules, plants: Plants) =
    val input = Source.fromResource(s"input$day.txt").getLines.toVector

    /** note that we only collect pot indices that contain a plant */
    val plants = input(0) match
      case s"initial state: $pots" => pots.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

    /** note that we only collect rules that yield a plant */
    val rules = input.drop(2).toSet.collect:
      case s"$pattern => $output" if output == "#" => pattern

    (rules, plants)

  extension (plants: Set[Int])

    /** returns the relevant surrounding pattern for given pot index */
    def pattern(index: PotIndex): String =
      (-2 to 2).map(i => if plants.contains(index + i) then '#' else '.').mkString

    /** returns the relevant range of plant containing pots */
    def pots: Range =
      plants.min - 2 to plants.max + 2

    /** returns the next generation of plant containing pot indices from that range for given rules */
    def next(rules: Rules): Set[Int] =
      pots.flatMap(pot => Option.when(rules.contains(pattern(pot)))(pot)).toSet

  def solve(plants: Plants, rules: Rules, generations: Long): Long =

    /** utilises the observation that the problem converses linearly from generation 102 and on */
    if generations >= 102 then
      @tailrec
      def go(plants: Plants, conversion: Int, generation: Int): Long =
        val next  = plants.next(rules)
        val delta = next.sum - plants.sum
        if delta == conversion then
          next.sum + delta * (generations - generation)
        else
          go(next, delta, generation + 1)
      go(plants, 0, 1)
    else
      Iterator.iterate(plants)(_.next(rules)).drop(generations.toInt).next.sum.toLong

  val start1  = System.currentTimeMillis
  val answer1 = solve(plants, rules, generations = 20)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = solve(plants, rules, generations = 50000000000L)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
