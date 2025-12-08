package aoc2021

import scala.util.*

  case class Vec(x: Int, y: Int):
    import Vec.*

    def +(that: Vec): Vec =
      add(this, that)

    def -(that: Vec): Vec =
      substract(this, that)

    def within(min: Vec, max: Vec): Boolean =
      min.x <= x && min.y <= y && x <= max.x && y <= max.y


  object Vec:

    val VecLit =
      """([-+]?\d+),([-+]?\d+),([-+]?\d+)""".r

    def apply(x: String, y: String): Vec =
      Vec(x.toInt, y.toInt)

    def apply(x: Char, y: Char): Vec =
      Vec(x.toString, y.toString)

    def parse(s: String): Vec =
      s match
        case VecLit(x, y) => Vec(x, y)
        case _            => sys.error(s"does not match ${VecLit.regex} : '$s'")

    val origin: Vec =
      Vec(0, 0)

    def add(a: Vec, b: Vec): Vec =
      Vec(a.x + b.x, a.y + b.y)

    def substract(a: Vec, b: Vec): Vec =
      Vec(b.x - a.x, b.y - a.y)

    def distance(a: Vec, b: Vec): Int =
      (b.x - a.x).abs + (b.y - a.y).abs

  case class Vec3(x: Int, y: Int, z: Int):
    import Vec3.*

    def +(that: Vec3): Vec3 =
      add(this, that)

    def -(that: Vec3): Vec3 =
      substract(this, that)

  object Vec3:

    val Vec3Lit =
      """([-+]?\d+),([-+]?\d+),([-+]?\d+)""".r

    def apply(x: String, y: String, z: String): Vec3 =
      Vec3(x.toInt, y.toInt, z.toInt)

    def apply(x: Char, y: Char, z: Char): Vec3 =
      Vec3(x.toString, y.toString, z.toString)

    def parse(s: String): Vec3 =
      s match
        case Vec3Lit(x, y, z) => Vec3(x, y, z)
        case _ => sys.error(s"does not match ${Vec3Lit.regex} : '$s'")

    val origin: Vec3 =
      Vec3(0, 0, 0)

    def add(a: Vec3, b: Vec3): Vec3 =
      Vec3(a.x + b.x, a.y + b.y, a.z + b.z)

    def substract(a: Vec3, b: Vec3): Vec3 =
      Vec3(b.x - a.x, b.y - a.y, b.z - a.z)

    def distance(a: Vec3, b: Vec3): Int =
      (b.x - a.x).abs + (b.y - a.y).abs + (b.z - a.z).abs
