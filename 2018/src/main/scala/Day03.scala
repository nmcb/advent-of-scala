import scala.io.Source

object Day03 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int)

  type Claims   = Vector[Vector[Pos]]
  type Overlaps = Map[Pos,Int]

  val (claims: Claims, overlaps: Overlaps) =
    val claims =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector
        .map:
          case s"#${id} @ ${x},${y}: ${w}x${h}" =>
            val x0 = x.toInt
            val x1 = w.toInt + x0
            val y0 = y.toInt
            val y1 = h.toInt + y0
            (for x <- x0 until x1 ; y <- y0 until y1 yield Pos(x,y)).toVector

    val overlaps = claims.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    (claims, overlaps)

  val start1  = System.currentTimeMillis
  lazy val answer1 = overlaps.valuesIterator.count(_ >= 2)
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  lazy val answer2 = 1 + claims.indexWhere(claim => claim.forall(pos => overlaps(pos) == 1))
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

