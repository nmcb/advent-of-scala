import scala.collection.*

object Day13 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):

    infix def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

    def isOpen: Boolean =
      val number = x*x + 3*x + 2*x*y + y + y*y + Pos.favorite
      val binary = number.toBinaryString
      binary.count(_ == '1') % 2 == 0

    def candidates: Vector[Pos] =
      Vector(Pos(1,0), Pos(-1,0), Pos(0,1), Pos(0,-1))
        .map(this + _)
        .filter(c => c.x >= 0 && c.y >= 0)

  object Pos:
    val favorite = 1352

  /** breadth first search */
  def solve1(start: Pos, target: Pos): Int =
    val todo    = mutable.Queue(start)
    val cache   = mutable.Map(start -> 0)
    var current = todo.dequeue

    while current != target do
      val steps = cache(current) + 1
      current.candidates.filter(_.isOpen).foreach: next =>
        if !cache.contains(next) || steps < cache(next) then
          cache(next) = steps
          todo.enqueue(next)
      current = todo.dequeue

    cache(target)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(start = Pos(1,1), target = Pos(31,39))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  /** breadth first search */
  def solve2(start: Pos): Map[Pos,Int] =
    val todo = mutable.Queue(start)
    val cache = mutable.Map(start -> 0)

    while todo.nonEmpty do
      val current = todo.dequeue
      val cost = cache(current) + 1
      current.candidates.filter(_.isOpen).foreach: next =>
        if (!cache.contains(next) || cost < cache(next)) && cost <= 50 then
          cache(next) = cost
          todo.enqueue(next)

    cache.toMap

  val start2  = System.currentTimeMillis
  val answer2 = solve2(start = Pos(1,1)).size
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
