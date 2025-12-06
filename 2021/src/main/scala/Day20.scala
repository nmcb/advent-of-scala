import scala.io.Source

object Day20 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    infix def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

  case class Image(pixels: Vector[Vector[Boolean]], canvas: Boolean)

  val offsets =
    for
      dy <- -1 to 1
      dx <- -1 to 1
    yield
      Pos(dx, dy)

  def within(grid: Vector[Vector[Boolean]], pos: Pos): Boolean =
    0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size

  def peek(grid: Vector[Vector[Boolean]], pos: Pos): Boolean =
    grid(pos.y)(pos.x)

  def int(binary: Vector[Boolean]): Int =
    binary.foldLeft(0)((acc, bit) => (acc << 1) | (if bit then 1 else 0))

  def enhance(algorithm: Vector[Boolean], image: Image): Image =
    val Image(pixels, canvas) = image
    val frame    = Vector.fill(pixels(0).size + 2)(canvas)
    val framed   = frame +: pixels.map(canvas +: _ :+ canvas) :+ frame
    val enhanced =
      for ((row, y) <- framed.zipWithIndex) yield
        for ((_, x) <- row.zipWithIndex) yield
          val pos  = Pos(x, y)
          val area = offsets.map(_ + pos)
          val bin  = area.map(cell => if within(framed, cell) then peek(framed, cell) else canvas)
          algorithm(int(bin.toVector))
  
    val Int9x0 = 0
    val Int9x1 = 511
    val background = algorithm(if canvas then Int9x1 else Int9x0)
    Image(enhanced, background)

  def parseImage(s: String): Image =
    Image(s.linesIterator.map(_.map(_ == '#').toVector).toVector, false)

  def parse(input: String) =
    val Vector(algorithmStr, imageStr) = input.split("\n\n").toVector
    val algorithm = algorithmStr.map(_ == '#').toVector
    val image = parseImage(imageStr)
    (algorithm, image)

  val (algorithm, image0) = parse(Source.fromResource(s"input$day.txt").mkString)

  val start1  = System.currentTimeMillis
  lazy val answer1 = enhance(algorithm, image0).pixels.map(_.count(_ == true)).sum
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  val start2  = System.currentTimeMillis
  lazy val answer2 = (1 to 50).foldLeft(image0)((img,_) => enhance(algorithm, img)).pixels.map(_.count(_ == true)).sum
  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
