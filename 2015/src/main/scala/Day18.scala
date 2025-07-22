import Day15.{Ingredient, day}

object Day18 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  /** Input */

  case class Light(underlying: Char):
    assert(underlying == '#' || underlying == '.')

    import Light.*

    def isOn:  Boolean = underlying == '#'
    def isOff: Boolean = underlying == '.'

    def next(neighbours: List[Light]): Light =
      val n = neighbours.count(_.isOn)

      if isOn then
        if n == 2 || n == 3 then on else off
      else
        if n == 3 then on else off


  object Light:
    val on: Light  = Light('#')
    val off: Light = Light('.')

    def fromChar(c: Char): Light =
      Light.apply(c)

  /** zero based left-to-right indexed, ie. [x0, x1 .. xn] */
  type Row = List[Light]

  object Row:
    def empty: Row =
      List.empty[Light]

  /** zero based top-to-bottom-left-to-right indexed, ie. [y0, y1 .. yn][x0, x1 .. xn] */
  type Conf = List[Row]

  object Conf:
    def empty: Conf =
      List.empty[Row]

  /** grid encapsulates the animation of a light configuration */
  case class Grid(conf: Conf, overlay: Set[(Int,Int)] = Set.empty):
    import Light.*

    val sizeX = conf.map(_.size).max
    val sizeY = conf.size
    val minX = 0
    val minY = 0
    val maxX = sizeX - 1
    val maxY = sizeY - 1

    def withOverlay(on: (Int,Int)*): Grid =
      copy(overlay = Set.from(on))

    def fold[A](zero: A)(inc: A => A)(f: (Int,Int,A) => A): A =
      (minY to maxY).foldLeft(zero)((z,y) =>
        (minX to maxX).foldLeft(inc(z))((z,x) =>
          f(x,y,z)))

    def mkString: String =
      fold("")(_ + "\n")((x,y,str) => str + light(x,y).underlying) + "\n"

    def count: Int =
      fold(0)(identity)((x,y,n) => if light(x, y).isOn then n + 1 else n)

    def light(x: Int, y: Int): Light =
      if overlay(y, x) then on else conf(y)(x)

    def neighbours(x: Int, y: Int): List[Light] =
      List((-1,-1),(0,-1),(1,-1),(-1, 0),(1, 0),(-1,1),(0, 1),(1, 1))
        .map((dx,dy) => (x + dx,y + dy))
        .filter((px,py) => px >= minX && px <= maxX && py >= minY && py <= maxY)
        .map(light)

    def next: Grid =
      val step: Conf =
        fold(Conf.empty)(_ :+ Row.empty)((x,y,conf) =>
          conf.init :+ (conf.last :+ light(x,y).next(neighbours(x,y))))
      copy(conf = step)

    def animate(steps: Int, grid: Grid = this): Grid =
      if steps <= 0 then grid else animate(steps = steps - 1, grid = grid.next)

  /** Input */

  val grid: Grid =
    import Light.*

    import scala.io.*
    val initial =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .foldLeft(Conf.empty)((conf,line) =>
          conf :+ line.foldLeft(Row.empty)((row, char) =>
            row :+ Light.fromChar(char)))
    Grid(initial)

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    grid
      .animate(100)
      .count

  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    import grid.*
    grid
      .withOverlay((minY,minX), (minY,maxX), (maxY,minX), (maxY,maxX))
      .animate(100)
      .count

  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
