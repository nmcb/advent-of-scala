package aoc2020

import nmcb.*
import scala.collection.*
import scala.util.boundary
import scala.util.boundary.break

object Day10 extends AoC:

  case class Adapter(rated: Int):

    def inputs: List[Int] =
      if      rated == 1 then List(0)
      else if rated == 2 then List(0,1)
      else                    List(rated - 1, rated - 2, rated - 3)

    def output: Int           = rated
    def diff(input: Int): Int = rated - input


  val ratings: List[Int] = lines.map(_.toInt).toList.sorted

  def solve1(ratings: List[Int]): Int =
    val (output, diff1, diff3) = 
      ratings
        .foldLeft((0,0,1)):
          case ((input,diff1,diff3),rated) =>
            val adapter = Adapter(rated)
            val (left, right) = if adapter.diff(input) == 1 then (diff1 + 1, diff3) else (diff1, diff3 + 1)
            (adapter.output, left, right)

    diff1 * diff3


  val sorted: List[Int] = ratings.sorted
  val search: List[Int] = (0 :: sorted) :+ (sorted.max + 3) // [0] :: as :: [max(as) + 3]
  val paths             = mutable.Map(0 -> 1L)              // V is # of paths from rating [0] to rating K
  for i <- 1 until search.length do                         // loop rating i from search(1) to rs(search.length-1)
    boundary:
      for j <- (0 to i).reverse do                          // loop rating j from rating(i) back to rating(0)

        // if diff ratings > 3 then break on j else upsert # paths of rating i adding # paths of j
        if search(i) - search(j) > 3 then
          break()
        else
          paths += i -> (paths.getOrElse(i, 0L) + paths.getOrElse(j, 0L))
  

  // # of rating configurations is # paths to last rating
  lazy val answer1: Int  = solve1(ratings)
  lazy val answer2: Long = paths(search.length - 1)
