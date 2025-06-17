import scala.collection.*

object Day11 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Cell = (Int,Int)

  extension (cell: Cell)
    def x: Int = cell._1
    def y: Int = cell._2
    def rackId: Int = x + 10

    infix def +(that: Cell): Cell = (cell.x + that.x, cell.y + that.y)

    def powerLevel: Int =
      var level = cell.rackId * cell.y
      level = level + 8561
      level = level * cell.rackId
      level = level.toString.dropRight(2).takeRight(1).toInt
      level - 5

    def asIdString(size: Int): String =
      s"${cell.x - size + 1},${cell.y - size + 1}"

  /* Note:
   *
   * Given a value table i(x,y) a summed area table can be computed as:
   *
   * I(x,y) = i(x,y) + I(x-1,y) + I(x,y-1) - I(x-1,y-1)
   *
   * After which the sum of an area can be computed as:
   *
   * A(x0,y0,x1,y1) = I(x0,y0) + I(x1,y1) - I(x1,y0) - I(x0,y1)
   */

  /** (x,y) cells for a 1 to 300 grid */
  val cells: Seq[Cell] =
    for y <- 1 to 300 ; x <- 1 to 300 yield (x,y)

  /** power level value table, i.e. the i(x,y) value table for named cells */
  val grid: Map[Cell,Int] =
    cells.map(c => c -> c.powerLevel).toMap

  /** summed power level table, i.e. the summed area table I(x,y) for named grid */
  val table: Map[Cell,Int] =
    cells.foldLeft(immutable.Map.empty[Cell,Int].withDefaultValue(0)): (result, cell) =>
      result + (cell -> (grid(cell) + result(cell + (-1,0)) + result(cell + (0,-1)) - result(cell + (-1,-1))))
  
  /** the area sizes, cells and total power levels for named summed power level table and given area size */
  def area(size: Int): Iterator[(Int,Cell,Int)] =
    for
      y <- (size to 300).iterator
      x <- (size to 300).iterator
    yield
      val cell  = (x,y)
      val total = table(cell) + table(cell + (-size,-size)) - table(cell + (-size,0)) - table(cell + (0,-size))
      (size, cell, total)

  extension (result: (Int,Cell,Int))
    def total: Int = result._3

  def solve1(): String =
    val (size, cell, total) = area(3).maxBy(_.total)
    cell.asIdString(size)

  val start1  = System.currentTimeMillis
  val answer1 = solve1()
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(): String =
    val (size, cell, total) = (for size <- (1 to 300).iterator yield area(size)).flatten.maxBy(_.total)
    s"${cell.asIdString(size)},${size.toString}"

  val start2  = System.currentTimeMillis
  val answer2 = solve2()
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
