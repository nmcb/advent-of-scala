import scala.io.*

object Day03 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Triangle = Vector[Int]

  extension (triangle: Triangle)
    def a: Int = triangle(0)
    def b: Int = triangle(1)
    def c: Int = triangle(2)

    def isValid: Boolean = triangle.permutations.forall(t => t.a + t.b > t.c)

  object Triangle:

    def fromLine(s: String): Vector[Int] =
      val sides = s.trim.split("\\s+")
      Vector(sides(0).toInt, sides(1).toInt, sides(2).toInt)

  val triangles: Vector[Triangle] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Triangle.fromLine)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = triangles.count(_.isValid)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def triangulate(todo: Vector[Int], result: Vector[Triangle] = Vector.empty): Vector[Triangle] =
    todo match
      case a +: b +: c +: rest => triangulate(rest, result :+ Vector(a, b, c))
      case _                   => result

  val start2  = System.currentTimeMillis
  val answer2 = triangulate(triangles.transpose.flatten).count(_.isValid)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
