import scala.io.*

object Day13 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Axis:
    case Ver, Hor

  import Axis.*

  type Dots  = Set[(Int, Int)]
  type Folds = Vector[(Axis, Int)]

  extension (dot: (Int, Int))
    def x: Int = dot._1
    def y: Int = dot._2

  val dots: Dots =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .collect:
        case s"$x,$y" => (x.toInt, y.toInt)
      .toSet
      
  val folds: Folds =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .collect:
        case s"fold along x=$x" => (Ver, x.toInt)
        case s"fold along y=$y" => (Hor, y.toInt)
      .toVector

  def origami(dots: Dots, folds: Folds): Dots =

    def vertical(line: Int)(x: Int, y: Int): (Int, Int) =
      (if x < line then x else 2 * line - x, y)

    def horizontal(line: Int)(x: Int, y: Int): (Int, Int) =
      (x, if y < line then y else 2 * line - y)

    folds.foldLeft(dots):
      case (dots, (Ver, line)) => dots.map(vertical(line))
      case (dots, (Hor, line)) => dots.map(horizontal(line))

  val start1  = System.currentTimeMillis
  lazy val answer1 = origami(dots, folds.take(1)).size
  println(s"Answer AOC 2021 day $day part 1 [${System.currentTimeMillis - start1}ms]: $answer1")

  extension (dots: Dots) def asString: String =
    val buffer = StringBuffer()
    for y <- 0 to dots.map(_.y).max do
      buffer.append('\n')
      for x <- 0 to dots.map(_.x).max do
        if dots.contains((x,y)) then
          buffer.append('#')
        else
          buffer.append('.')
    buffer.toString + "\n"

  val start2  = System.currentTimeMillis
  lazy val answer2 = origami(dots, folds).asString
  println(s"Answer AOC 2021 day $day part 2 [${System.currentTimeMillis - start1}ms]: $answer2")
