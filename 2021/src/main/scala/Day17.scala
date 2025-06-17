import scala.annotation.tailrec

object Day17 extends App:
  
  val day = getClass.getSimpleName.filter(_.isDigit).mkString
  
  case class Probe(x: Int, y: Int, vx: Int, vy: Int):
    def step: Probe =
      val nx  = x + vx
      val ny  = y + vy
      val nvx = if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0
      val nvy = vy - 1
      Probe(nx, ny, nvx, nvy)

  val targetX =  150 to 193
  val targetY = -136 to -86

  @tailrec
  def go(x: Int, y: Int, vx: Int, vy: Int, maxY: Int = 0): Option[Int] =
    Probe(x, y ,vx, vy).step match
      case Probe(nx, ny, nvx, nvy) if targetX.contains(nx) && targetY.contains(ny) => Some(maxY)
      case Probe(nx, ny, nvx, nvy) if nx > targetX.max => None
      case Probe(nx, ny, nvx, nvy) if ny < targetY.min => None
      case Probe(nx, ny, nvx, nvy) => go(nx, ny, nvx, nvy, if ny > maxY then ny else maxY)

  val start1  = System.currentTimeMillis
  val answer1 = (0 to 50).flatMap(vx => (0 to 200).flatMap(vy => go(0, 0, vx, vy))).max
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  
  val start2  = System.currentTimeMillis
  val answer2 = (0 to 200).flatMap(vx => (-200 to 200).flatMap(vy => go(0, 0, vx, vy))).size
  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
