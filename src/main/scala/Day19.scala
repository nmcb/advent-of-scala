import scala.annotation.tailrec
import scala.io.*

object Day19 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit).mkString

  import vector.*

  case class Scanner(idx: Int, report: Set[Vec3])

  val scanners: Vector[Scanner] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList
      .foldLeft(Vector.empty[Vector[Vec3]])((a,l) =>
        if l.startsWith("--- scanner") then
          Vector.empty[Vec3] +: a
        else if l.isEmpty then
          a
        else
          (Vec3.parse(l) +: a.head) +: a.tail
      )
      .reverse
      .zipWithIndex
      .map((report,index) => Scanner(index, report.toSet))

  def orientations(pos: Vec3): Vector[Vec3] =
    val Vec3(x, y, z) = pos
    Vector(
         Vec3(+x,+y,+z), Vec3(-y,+x,+z), Vec3(-x,-y,+z), Vec3(+y,-x,+z)
       , Vec3(-x,+y,-z), Vec3(+y,+x,-z), Vec3(+x,-y,-z), Vec3(-y,-x,-z)
       , Vec3(-z,+y,+x), Vec3(-z,+x,-y), Vec3(-z,-y,-x), Vec3(-z,-x,+y)
       , Vec3(+z,+y,-x), Vec3(+z,+x,+y), Vec3(+z,-y,+x), Vec3(+z,-x,-y)
       , Vec3(+x,-z,+y), Vec3(-y,-z,+x), Vec3(-x,-z,-y), Vec3(+y,-z,-x)
       , Vec3(+x,+z,-y), Vec3(-y,+z,-x), Vec3(-x,+z,+y), Vec3(+y,+z,+x)
       )

  def find(beacons: Set[Vec3], scanner: Scanner): Option[(Set[Vec3], Vec3)] =
    val result =
      for
        transposed <- scanner.report.map(orientations).transpose.map(_.toSet)
        local      <- beacons
        remote     <- transposed
        position = remote - local
        if transposed.map(position + _).count(beacons) >= 10
      yield (transposed.map(position + _), position)
    
    result.headOption
  
  def solve(scanners: Vector[Scanner]): (Set[Vec3], Set[Vec3]) =

    @tailrec
    def go(todo: Vector[Scanner], known: Set[Vec3], found: Set[Vec3], scanners: Set[Vec3]): (Set[Vec3], Set[Vec3]) =
      val beacons = known ++ found
      if todo.isEmpty then
        (beacons, scanners)
      else
        val result =
          for
            scanner                 <- todo
            (transposed, positions) <- find(found, scanner)
          yield (scanner, transposed, positions)
        val matched   = result.map(_._1)
        val oriented  = result.map(_._2)
        val positions = result.map(_._3)
          
        val next = todo.filterNot(matched.contains)
        val pack = if oriented.nonEmpty then oriented.reduce(_ ++ _) else Set.empty
        go(next, beacons, pack, scanners ++ positions)

    go(scanners.tail, Set.empty, scanners.head.report, Set(Vec3.origin))

  val start1   = System.currentTimeMillis
  val (beacons, positions) = solve(scanners)
  val answer1 = beacons.size
  println(s"answer 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = (for a <- positions ; b <- positions yield Vec3.distance(b, a)).max
  println(s"answer 2: $answer2 [${System.currentTimeMillis - start2}ms]")


  

