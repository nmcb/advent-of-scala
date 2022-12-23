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
    val rocks: LazyList[Rock] = LazyList(Min, Plus, El, Stack, Box) #::: rocks

  var rockCount: Int = 0

  case class Chamber(pattern: LazyList[Move], stopped: List[(Pos,Rock)]):
    def width: Int = 7
    def height: Int = if isEmpty then Pos.origin.y else tower.map(_.y).max + 1
    def tower: List[Pos] = stopped.take(100).flatMap((p,r) => r.withOrigin(p))
    def isWall(p: Pos): Boolean     = p.x < Pos.origin.x || p.x >= Pos.origin.x + width
    def isFloor(p: Pos): Boolean    = p.y < Pos.origin.y
    def isOccupied(p: Pos): Boolean = isWall(p) || isFloor(p) || tower.contains(p)
    def isEmpty: Boolean = stopped.isEmpty

    def next(rock: Rock): Chamber =

      def appear: Pos = Pos.origin.translate(dx = 2, dy = height + 3)

      def loop(p: Pos, moves: LazyList[Move], a: List[(Move,Option[Pos])] = List.empty): List[(Move,Option[Pos])] =
        val (m,n) = moves.head match
          case L => (L, p.translate(dx = -1, dy =  0))
          case R => (R, p.translate(dx =  1, dy =  0))
          case D => (D, p.translate(dx =  0, dy = -1))

        if rock.withOrigin(n).forall(p => !isOccupied(p)) then
          if m == L || m == R then
            loop(n, D #:: moves.tail, (m, Some(n)) :: a)
          else
            loop(n, moves.tail, (m, Some(n)) :: a)
        else
          if m == L || m == R then
            loop(p, D #:: moves.tail, (m, None) :: a)
          else
            a

      val ms = loop(appear, pattern)
      val ns =
        ms.dropWhile((_,op) => op.isEmpty)
          .headOption
          .flatMap((_,op) => op.map(p => (p,rock) :: stopped))
          .getOrElse(stopped)
      val np = pattern.drop(ms.filterNot((m,_) => m == D).size)
      if ms.nonEmpty then Chamber(pattern = np, stopped = ns) else this

  object Chamber:
    def empty: Chamber = Chamber(pattern = pattern, stopped = List.empty)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Rocks.rocks.take(2022).foldLeft(Chamber.empty)(_ next _).height
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  type Nr = Int

  val nr: Nr = 10000
  val stack: List[(Pos,Rock)] = Rocks.rocks.take(nr).foldLeft(Chamber.empty)(_ next _).stopped.reverse

  println(stack.take(10).mkString("\n"))

  def row(y: Int): List[Option[(Rock,Nr)]] =
    val xWithRockNrForY: Map[Int,(Rock,Nr)] =
      stack
        .zipWithIndex
        .map{ case (or,i) => or -> i }
        .flatMap{ case ((o,r),nr) => r.withOrigin(o).map(p => p -> (r, nr)) }
        .filter{ case (p,_) => p.y == y }
        .map((p,rnr) => p.x -> rnr)
        .toMap
    (0 until 7).foldLeft(List.empty)((a,x) => {
      a :+ xWithRockNrForY.get(x).orElse(None)
    })

  def floors: List[Int] =
    stack
      .flatMap((p,r) => r.withOrigin(p))
      .groupMap(_.y)(_.x)
      .filter(_._2.size == 7)
      .map(_._1)
      .toList
      .sorted

  println(s"floors=${floors.take(100).mkString("\n","\n", "\n")}")

  def diff: List[Int] =
    floors.zip(floors.tail).map((f1,f2) => f2 - f1)

  println(s"diff=${diff.take(100).mkString("\n","+","\n")}")

  def startY: Int = 655
  println(s"startY=$startY")
  def cycleY: Int = 101+237+32+280+56+197+226+126+6+30+18+127+113+483+20+407+129+97
  println(s"cycleY=$cycleY")

  for (y <- startY to startY) println(s"y=$y - ${row(y)}")
  for (y <- startY + cycleY to startY + cycleY) println(s"y=$y - ${row(y)}")
  for (y <- startY + cycleY * 2 to startY + cycleY * 2) println(s"y=$y - ${row(y)}")

  def startNr: Int = row(startY).flatMap(_.map(_._2)).max
  println(s"startNr=$startNr")
  def cycleNr: Int = row(startY + cycleY).flatMap(_.map(_._2)).max - startNr
  println(s"cycleNr=$cycleNr")

  def totalNr: Long = 1000_000_000_000
  println(s"totalNr=$totalNr")
  def restNr: Long = (totalNr - startNr) % cycleNr
  println(s"restNr=$restNr")
  def cycleSize: Long = (totalNr - startNr - restNr) / cycleNr
  println(s"cycleSize=$cycleSize")
  def endY: Long = startY + cycleY * cycleSize
  println(s"endY=$endY")
  def deltaY: Long = Rocks.rocks.take((startNr + restNr).toInt).foldLeft(Chamber.empty)(_ next _).height - startY
  println(s"deltaY=$deltaY")
  def totalY: Long = endY + deltaY
  println(s"totalY=$totalY")

  assert(totalY == 1556521739139L)
//
//  val goedeAntwoord: Long =
//    1584927536247L
//    1584927536247L
//    1590459569509L






//  val start2: Long = System.currentTimeMillis
//  val answer2 = 666 // im dead in the water - swimming cycles
//  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
//
//  /** Utilities */
//
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
