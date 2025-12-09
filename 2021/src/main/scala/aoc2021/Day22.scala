package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day22 extends AoC:

  case class CuboidStep(on: Boolean, xr: Range, yr: Range, zr: Range)

  val StepLit =
    """(on|off) x=([-+]?\d+)..([-+]?\d+),y=([-+]?\d+)..([-+]?\d+),z=([-+]?\d+)..([-+]?\d+)""".r

  val cuboidSteps: Vector[CuboidStep] =
    lines
      .map:
        case StepLit(set, x0, x1, y0, y1, z0, z1) =>
          import math.*
          val xr = min(x0.toInt, x1.toInt) to max(x0.toInt, x1.toInt)
          val yr = min(y0.toInt, y1.toInt) to max(y0.toInt, y1.toInt)
          val zr = min(z0.toInt, z1.toInt) to max(z0.toInt, z1.toInt)
          val on = set == "on" 
          CuboidStep(on, xr, yr, zr)
      .toVector

  case class Pos(x: Int, y: Int, z: Int):

    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y, z + that.z)

    def <=(that: Pos): Boolean =
      x <= that.x && y <= that.y && z <= that.z

    def min(that: Pos): Pos =
      Pos(x.min(that.x), y.min(that.y), z.min(that.z))

    def max(that: Pos): Pos =
      Pos(x.max(that.x), y.max(that.y), z.max(that.z))


  case class Cuboid(all: Map[Pos,Boolean]):

    def getOrElse(p: Pos, b: Boolean): Boolean =
      all.getOrElse(p, b)

    def update(pos: Pos, bool: Boolean): Cuboid =
      map((p,b) => if (pos == p) bool else b)

    def update(g: Cuboid): Cuboid =
      map((p,b) => g.getOrElse(p, b))

    def map(f: (Pos,Boolean) => Boolean): Cuboid =
      Cuboid(all.map((p, a) => p -> f(p,a)))

  object Cuboid:

    val init = cuboid(-50 to 50, -50 to 50, -50 to 50)(false)

    def cuboid(xr: Range, yr: Range, zr: Range)(b: Boolean): Cuboid =
      val all =
        for
          x <- xr
          y <- yr
          z <- zr
        yield
          Pos(x, y, z)
      Cuboid(all.groupMapReduce(identity)(_ => b)(_ && _))

    def cuboid(s: CuboidStep): Cuboid =
      cuboid(s.xr, s.yr, s.zr)(s.on)

    def inside(r: Range): Boolean =
      r.start >= -50 && r.end <= 50

    def inside(s: CuboidStep): Boolean =
      inside(s.xr) && inside(s.yr) && inside(s.zr)

    def reboot(steps: Vector[CuboidStep], grid: Cuboid = init): Cuboid =
      steps.filter(inside).foldLeft(grid)((g,s) => g.update(cuboid(s)))

  case class Cube(min: Pos, max: Pos): 

    def size: Long =
      val xs = max.x - min.x + 1L
      val ys = max.y - min.y + 1L
      val zs = max.z - min.z + 1L
      xs * ys * zs

    infix def union(that: Cube): Cube =
      val minMin = min.min(that.min)
      val maxMax = max.max(that.max)
      Cube(minMin, maxMax)

    infix def intersect(that: Cube): Option[Cube] =
      val maxMin = min.max(that.min)
      val minMax = max.min(that.max)
      Option.when(maxMin <= minMax)(Cube(maxMin, minMax))

  case class CubeStep(on: Boolean, cube: Cube)
  
  object Cube:

    def reboot(steps: Vector[CubeStep]): Long =

      @tailrec
      def go(splits: Vector[(CubeStep,Set[Int])], signum: Int, found: Long): Long =
        val count = splits.map((s,_) => if s.on then s.cube.size else 0L).sum
        val acc   = found + signum * count

        if splits.nonEmpty then
          val todo: Vector[(CubeStep,Set[Int])] =
            for
              (split, indices) <- splits
              (step, index)    <- steps.zipWithIndex
              if index > indices.max && (split.on || !step.on)
              intesection      <- split.cube intersect step.cube
            yield
              (CubeStep(step.on || split.on, intesection), indices + index)
          go(todo, -signum, acc)
        else
          acc

      val init = steps.zipWithIndex.map((step,index) => (step, Set(index)))
      go(init, +1, 0L)

  lazy val cubeSteps: Vector[CubeStep] =
    lines
      .map:
        case StepLit(on, x0, x1, y0, y1, z0, z1) =>
          val min = Pos(x0.toInt, y0.toInt, z0.toInt)
          val max = Pos(x1.toInt, y1.toInt, z1.toInt)
          val set = on == "on"
          CubeStep(set, Cube(min, max))

  lazy val answer1: Int  = Cuboid.reboot(cuboidSteps).all.values.count(identity)
  lazy val answer2: Long = Cube.reboot(cubeSteps)
