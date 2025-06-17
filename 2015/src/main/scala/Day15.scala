object Day15 extends App:

  val day: String =
    this.getClass.getName.drop(3).init


  /** Modeling */

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  object Ingredient:

    def fromString(s: String): Ingredient =
      s match
        case s"$name: capacity $capacity, durability $durability, flavor $flavor, texture $texture, calories $calories" =>
          Ingredient( name       = name
                    , capacity   = capacity.toInt
                    , durability = durability.toInt
                    , flavor     = flavor.toInt
                    , texture    = texture.toInt
                    , calories   = calories.toInt
                    )

  case class Recipe(measurements: Map[Ingredient,Int]):

    def score: Int =

      def scores(ingredient: Ingredient, spoons: Int): List[Int] =
        List( ingredient.capacity   * spoons
            , ingredient.durability * spoons
            , ingredient.flavor     * spoons
            , ingredient.texture    * spoons
            )

      measurements
        .map(scores)
        .fold(List.empty[Int])((total,current) => if total.isEmpty then current else total.zip(current).map(_ + _))
        .map(score => if score <= 0 then 0 else score)
        .product

    def calories: Int =
      measurements
        .map((ingredient, spoons) => ingredient.calories * spoons)
        .sum



  /** Input */

  val MaxSpoons: Int = 100

  val ingredients: List[Ingredient] =
    scala.io.Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Ingredient.fromString)
      .toList

  val recipes: List[Recipe] =
    ingredients
      .flatMap(i => List.fill(MaxSpoons)(i))
      .combinations(MaxSpoons)
      .map(_.groupBy(identity).view.mapValues(_.size).toMap)
      .map(Recipe.apply)
      .toList


  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    recipes.map(_.score).max

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    recipes.filter(_.calories == 500).map(_.score).max

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
