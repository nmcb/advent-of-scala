import scala.io.Source

object Day21 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Grid = Vector[String]

  extension (grid: Grid)

    def flipH: Grid =
      grid.map(_.reverse)

    def flipV: Grid =
      grid.reverse

    def rotate90: Grid =
      grid.transpose.map(_.mkString("").reverse)

    def rotate180: Grid =
      rotate90.rotate90

    def rotate270: Grid =
      rotate180.rotate90

    def chop(n: Int): Vector[Vector[Grid]] =
      val chopSize = grid.size / n
      for y <- (0 until chopSize).toVector yield
      for x <- (0 until chopSize).toVector yield
        val chopped =
          for
            iy <- 0 until n
            ix <- 0 until n
          yield
            grid(y * n + iy)(x * n + ix)
        Grid.fromIndexedSeq(chopped)

    def evenlyDivisibleBy2: Boolean =
      grid.size % 2 == 0

    def apply(rules: Vector[Rule]): Grid =
      rules.find(_.applies(grid)).map(_.output).getOrElse(sys.error(s"no rule applies: $grid"))

  object Grid:

    def fromString(s: String): Grid =
      val size = math.sqrt(s.filterNot(_ == '/').length).toInt
      s.grouped(size).toVector

    def fromIndexedSeq(s: IndexedSeq[Char]): Grid =
      fromString(s.mkString(""))

    val flips: Vector[Grid => Grid] =
      Vector(identity, flipV, flipH)

    val rotations: Vector[Grid => Grid] =
      Vector(identity, rotate90, rotate180, rotate270)

    val transformations: Vector[Grid => Grid] =
      flips.flatMap(flip => rotations.map(rotate => flip.andThen(rotate)))

    val init: Grid =
      fromString(".#...####")

  case class Rule(pattern: Grid, output: Grid):

    val patterns: Set[Grid] =
      Grid.transformations.map(transformation => transformation(pattern)).toSet

    def applies(grid: Grid): Boolean =
      patterns.contains(grid)

  object Rule:

    def fromLine(s: String): Rule =
      s.filterNot(_ == '/') match
        case s"$p => $o" => Rule(Grid.fromString(p), Grid.fromString(o))

  private val rules: Vector[Rule] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Rule.fromLine)
      .toVector

  extension (chopped: Vector[Vector[Grid]])

    def mapGrid(f: Grid => Grid): Vector[Vector[Grid]] =
      chopped.map(_.map(f))

    def toGrid: Grid =

      val choppedGridSize: Int =
        chopped.head.head.size

      val gridSize: Int =
        chopped.size * choppedGridSize

      val chars: IndexedSeq[Char] =
        for
          y <- 0 until gridSize
          x <- 0 until gridSize
        yield
          chopped(y / choppedGridSize)(x / choppedGridSize)(y % choppedGridSize)(x % choppedGridSize)

      Grid.fromIndexedSeq(chars)

  def solve1(grid: Grid, allRules: Vector[Rule], iterations: Int): Int =
    def loop(iteration: Int, accumulator: Grid): Int =
      if iteration >= iterations then
        accumulator.flatten.count(_ == '#')
      else
        val chopSize = if accumulator.size % 2 == 0 then 2 else 3
        val rules    = allRules.filter(_.pattern.size == chopSize)
        val next     = accumulator.chop(chopSize).mapGrid(_.apply(rules)).toGrid
        loop(iteration = iteration + 1, accumulator = next)
    loop(0, grid)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve1(Grid.init, rules, 5)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve1(Grid.init, rules, 18)
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
