import scala.annotation.*
import scala.io.*
import scala.math.*

object Day17 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val input: List[Move] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .toList

  val pattern: LazyList[Move] =
      input.to(LazyList) #::: pattern

  case class Pos(x: Int, y: Int):
    def translate(dx: Int, dy: Int): Pos = Pos(x + dx, y + dy)

  object Pos:
    val origin: Pos = Pos(0,0)

  type Move = Char
  val L = '<'
  val R = '>'
  val D = 'v'

  sealed abstract class Rock(val relative: List[Pos]):
    def withOrigin(o: Pos): List[Pos] =
      relative.map(p => Pos(p.x + o.x, p.y + o.y))

  case object Min   extends Rock(List(Pos(0,0),Pos(1,0),Pos(2,0),Pos(3,0)))
  case object Plus  extends Rock(List(Pos(1,0),Pos(0,1),Pos(1,1),Pos(2,1),Pos(1,2)))
  case object El    extends Rock(List(Pos(0,0),Pos(1,0),Pos(2,0),Pos(2,1),Pos(2,2)))
  case object Stack extends Rock(List(Pos(0,0),Pos(0,1),Pos(0,2),Pos(0,3)))
  case object Box   extends Rock(List(Pos(0,0),Pos(1,0),Pos(0,1),Pos(1,1)))

  object Rocks:
    var rocks: List[Rock] = List(Min, Plus, El, Stack, Box)
    def next: Rock = { val r = rocks.head ; rocks = rocks.tail :+ r ; r }
    def take(n: Int): List[Rock] = List.fill(n)(next)

  var rockCount: Int = 0

  case class Chamber(pattern: LazyList[Move], stopped: List[(Pos,Rock)]):
    def width: Int = 7
    def height: Int = if isEmpty then Pos.origin.y else tower.map(_.y).max + 1
    def tower: List[Pos] = stopped.take(100).flatMap((p,r) => r.withOrigin(p))
    def isWall(p: Pos): Boolean     = p.x < Pos.origin.x || p.x >= Pos.origin.x + width
    def isFloor(p: Pos): Boolean    = p.y < Pos.origin.y
    def isOccupied(p: Pos): Boolean = isWall(p) || isFloor(p) || tower.contains(p)
    def isEmpty: Boolean = stopped.isEmpty

    lazy val floors: List[Int] =
      stopped
        .flatMap((p,r) => r.withOrigin(p))
        .groupMap(_.y)(_.x)
        .filter(_._2.size == 7)
        .map(_._1)
        .toList

    lazy val posToRockNrs: List[(Int,Seq[Int])] =
      stopped
        .reverse
        .zipWithIndex
        .flatMap{ case ((o,r),i) => r.withOrigin(o).sortBy(_.x).map(p => p -> i) }
        .groupMap(_._1.y)(x => x._2)
        .toList
        .sortBy(_._1)
        .reverse
//        .sorted
//        .grouped(2)
//        .map(d => d(1) - d(0))
//        .toList



    lazy val cycle: Int = 9
    lazy val floor1: Int = floors.head
    lazy val floorD: Int = floors.tail.take(cycle).sum

    def next(rock: Rock): Chamber =

      def appear: Pos = Pos.origin.translate(dx = 2, dy = height + 3)

      def loop(p: Pos, moves: LazyList[Move], a: List[(Move,Option[Pos])] = List.empty): List[(Move,Option[Pos])] =
        val (m,n) = moves.head match
          case L => (L, p.translate(dx = -1, dy =  0))
          case R => (R, p.translate(dx =  1, dy =  0))
          case D => (D, p.translate(dx =  0, dy = -1))

        if rock.withOrigin(n).forall(p => !isOccupied(p)) then
          if m == L || m == R then
            loop(n, D #:: moves.tail, (m,Some(n)) :: a)
          else
            loop(n, moves.tail, (m,Some(n)) :: a)
        else
          if m == L || m == R then
            loop(p, D #:: moves.tail, (m,None) :: a)
          else
            a

      val ms = loop(appear, pattern)
      val ns = ms.dropWhile((_,op) => op.isEmpty).headOption.flatMap((_,op) => op.map(p => (p,rock) :: stopped)).getOrElse(stopped)
      val np = pattern.drop(ms.filterNot((m,_) => m == D).size)
      if ms.nonEmpty then Chamber(pattern = np, stopped = ns) else this

  object Chamber:
    def empty: Chamber = Chamber(pattern = pattern, stopped = List.empty)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Rocks.take(2022).foldLeft(Chamber.empty)(_ next _).height
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val rocks = 100000
  val c = Rocks.take(rocks).foldLeft(Chamber.empty)(_ next _)
  println(s"height=${c.height}")
  println(s"floor1=${c.floor1}")
  println(s"floorD=${c.floorD}")


  def pr(count: Int = 0, l: List[(Int,Seq[Int])] = c.posToRockNrs): Unit =
    val h = l.head
    val t = h._2.size == 7
    println(s"${h._1} - ${h._2.mkString(",")} ($t)")
    if count == 1000 then () else pr(count - 1, l.tail)

  pr()


  val start2: Long = System.currentTimeMillis
  val answer2 = 666 // im dead in the water - swimming cycles
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

  /** Utilities */

//  def brent[A](f: A => A, z: A): (A, A) = {
//    val lambda = findLambda(f, z)
//    val mu = findMu(f, z, lambda)
//    (lambda, mu)
//  }
//
//  def cycle[A](f: A => A, z: A)(given unit: Monoid[A]): Seq[A] = {
//    val (lambda, mu) = brent(f, z)
//    (unit.Z until mu + lambda)
//      .foldLeft(Seq(z))((list, _) => f(list.head) +: list)
//      .reverse
//      .drop(mu)
//  }
//
//  def findLambda(f: Int => Int, x0: Int): Int = {
//    findLambdaRec(f, tortoise = x0, hare = f(x0), power = 1, lambda = 1)
//  }
//
//  def findMu(f: Int => Int, x0: Int, lambda: Int): Int = {
//    val hare = (0 until lambda).foldLeft(x0)((x, _) => f(x))
//    findMuRec(f, tortoise = x0, hare, mu = 0)
//  }
//
//  @tailrec
//  private def findLambdaRec(f: Int => Int, tortoise: Int, hare: Int, power: Int, lambda: Int): Int = {
//    if (tortoise == hare) {
//      lambda
//    } else {
//      val (newTortoise, newPower, newLambda) = if (power == lambda) {
//        (hare, power * 2, 0)
//      } else {
//        (tortoise, power, lambda)
//      }
//      findLambdaRec(f, newTortoise, f(hare), newPower, newLambda + 1)
//    }
//  }
//
//  @tailrec
//  private def findMuRec(f: Int => Int, tortoise: Int, hare: Int, mu: Int): Int = {
//    if (tortoise == hare) {
//      mu
//    } else {
//      findMuRec(f, f(tortoise), f(hare), mu + 1)
//    }
//  }
