package aoc2020

import nmcb.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day21 extends AoC:

  type Ingredient  = String
  type Allergen    = String
  type Ingredients = Set[Ingredient]
  type Allergens   = Set[Allergen]
  type Input       = Vector[(Ingredients, Allergens)]

  val regex: Regex = "(.+) \\(contains (.+)\\)".r

  def partition(input: Input): (Map[Ingredient,Allergens], Map[Allergen,Ingredients]) =

    val candidates =
      input.foldLeft(Map.empty[Ingredient,Allergens]):
        case (candidates, (ingredients, allergens)) =>
          ingredients.foldLeft(candidates): (candidates, ingredient) =>
            candidates.updated(ingredient, candidates.getOrElse(ingredient, Set.empty) ++ allergens)

    val reduced =
      candidates.map: (candidate, possible) =>
        val next = input.foldLeft(possible):
          case (possible, (ingredients, allergens)) =>
            if ingredients.contains(candidate) then possible else possible -- allergens
        (candidate, next)


    reduced.partition((_, possible) => possible.isEmpty)

  def findIngredient(risky: Map[Ingredient,Allergens]): Ingredient =

    extension (t: (String,String))
      def ingredient: String = t._1
      def allergen: String   = t._2

    @tailrec
    def go(remaining: Map[Ingredient,Allergens], known: Vector[(Ingredient,Allergen)]): Vector[(Ingredient,Allergen)] =
      if remaining.isEmpty then
        known
      else
        val (ingredient, allergen) = remaining.find((_, allergens) => allergens.size == 1).get
        val todo  = remaining.removed(ingredient).view.mapValues(_ - allergen.head).toMap
        val found = (ingredient, allergen.head)
        go(todo, found +: known)

    go(risky, Vector.empty).sortBy(_.allergen).map(_.ingredient).mkString(",")

  val puzzle: Input = lines.map:
    case regex(ingredients, allergens) => (ingredients.split(" ").toSet, allergens.split(", ").toSet)


  def part1(input: Input): Int =
    val (inert,risky) = partition(input)
    input.map((ingredients,_) => inert.keySet.intersect(ingredients).size).sum

  def part2(input: Input): String =
    val (inert, risky) = partition(input)
    findIngredient(risky)


  lazy val answer1: Int    = part1(puzzle)
  lazy val answer2: String = part2(puzzle)
