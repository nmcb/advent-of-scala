import Day10.KnotHash
import Day12.Dijkstra

object Day14 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  extension (knot: KnotHash)

    def toBinary: Vector[Int] =
      knot
        .denseHash
        .flatMap: bin =>
          Vector(bin >> 4 & 0x0f, bin & 0x0f)
        .foldLeft(Vector.empty[Int]): (acc, bin) =>
          acc ++ Vector(bin >> 3 & 0x01, bin >> 2 & 0x01, bin >> 1 & 0x01, bin >> 0 & 0x01)

    def toBinaryString: String =
      knot.toBinary.mkString("")

  val hashes: IndexedSeq[String] =
    for
      i <- 0 until 128
    yield
      KnotHash.compute(s"uugsqrei-$i", runs = 64).toBinaryString

  val start1: Long = System.currentTimeMillis
  val answer1: Int = hashes.mkString("").count(_ == '1')
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  case class Pos(x: Int, y: Int):

    infix def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

    def adjacent: Set[Pos] =
      Set(Pos(0,1),Pos(0,-1),Pos(1,0),Pos(-1,0))
        .map(this + _)
        .filter(p => p.x >= 0 && p.x < 128 && p.y >= 0 && p.y < 128)

  val used: Set[Pos] =
    hashes
      .map(_.zipWithIndex)
      .zipWithIndex
      .foldLeft(Set.empty[Pos]):
        case (acc,(row, y)) =>
          row.foldLeft(acc):
            case (acc, (square, x)) =>
              if square == '1' then acc + Pos(x,y) else acc

  def regions(used: Set[Pos]): Set[Set[Pos]] =
    val graph = used.map(square => square -> square.adjacent.intersect(used)).toMap
    def loop(todo: Set[Pos], regions: Set[Set[Pos]] = Set.empty): Set[Set[Pos]] =
      if todo.isEmpty then
        regions
      else
        val square = todo.head
        val region = Dijkstra.reachable(square, graph)
        loop(todo -- region - square, regions + region)
    loop(used)

  val start2: Long = System.currentTimeMillis
  val answer2: Int = regions(used).size
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
