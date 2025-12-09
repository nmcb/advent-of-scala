package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day17 extends AoC:
  
  case class Probe(x: Int, y: Int, vx: Int, vy: Int):
    def step: Probe =
      val nx  = x + vx
      val ny  = y + vy
      val nvx = if vx > 0 then vx - 1 else if vx < 0 then vx + 1 else 0
      val nvy = vy - 1
      Probe(nx, ny, nvx, nvy)

  val targetX: Seq[Int] =  150 to 193
  val targetY: Seq[Int] = -136 to -86

  @tailrec
  def go(x: Int, y: Int, vx: Int, vy: Int, maxY: Int = 0): Option[Int] =
    Probe(x, y ,vx, vy).step match
      case Probe(nx, ny, nvx, nvy) if targetX.contains(nx) && targetY.contains(ny) => Some(maxY)
      case Probe(nx, ny, nvx, nvy) if nx > targetX.max => None
      case Probe(nx, ny, nvx, nvy) if ny < targetY.min => None
      case Probe(nx, ny, nvx, nvy) => go(nx, ny, nvx, nvy, if ny > maxY then ny else maxY)


  lazy val answer1: Int = (0 to 50).flatMap(vx => (0 to 200).flatMap(vy => go(0, 0, vx, vy))).max
  lazy val answer2: Int = (0 to 200).flatMap(vx => (-200 to 200).flatMap(vy => go(0, 0, vx, vy))).size
