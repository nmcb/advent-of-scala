package aoc2023

import nmcb.*

import scala.math

object Day24 extends AoC:

  val stones: Vector[Stone] = lines.map(Stone.fromString)

  def solve1(min: Long, max: Long): Int =
    stones
      .combinations(2)
      .flatMap: combination =>
        combination(0).futureIntersect(combination(1), planeXY)
      .count: (x, y, _) =>
        x >= min && x <= max && y >= min && y <= max


  /** Attempt 1 - Didn't work
   *  ------------------------------------------------------------------------------------------------------
   *  Find two hailstones flying on parallel lines, two parallel lines define a plane in 3D space. Find two
   *  others, they define another plane, two planes intersecting define exactly one line which has to be the
   *  path the rock takes. Calculate intersection point for one hailstone + the line you just solved, find
   *  milliseconds for hail reaching said line from hailstone's initial position + velocity. Repeat for a
   *  different hailstone to get a different time, then work backwards from those times to figure out the
   *  velocity for your rock. After you have the velocity and the direction the rock is travelling on, you
   *  can iterate through all hailstones one last time to find the position needed on 0 milliseconds.
   */
  object Attempt1:
    def parallelInPlane(plane: Plane): (Stone, Stone) =
      stones
        .combinations(2)
        .find: combination =>
          combination(0).parallel(combination(1), plane)
        .map(parallel => parallel(0) -> parallel(1))
        .getOrElse(sys.error(s"input error"))

    def rockVelocityInPlane(stone1: Stone, stone2: Stone, plane: Plane, unit: Long => Vec): Vec =
      Iterator
        .from(0)
        .map(_.toLong)
        .map(unit)
        .find: offset =>
          val relativeToInertRock1 = stone1.copy(velocity = stone1.velocity - offset)
          val relativeToInertRock2 = stone2.copy(velocity = stone2.velocity - offset)
          relativeToInertRock1.futureIntersect(relativeToInertRock2, plane).isDefined
        .getOrElse(sys.error("input error"))

    def collisionTimeRelativeToRockVelocity(rockVector: Vec): Double =
      stones
        .combinations(2)
        .map: combination =>
          val relativeToInertRock1 = combination(0).copy(velocity = combination(0).velocity - rockVector)
          val relativeToInertRock2 = combination(1).copy(velocity = combination(1).velocity - rockVector)
          relativeToInertRock1.futureIntersect(relativeToInertRock2, planeXY).map((x, y, time) => time) // or plane XZ, or plane YZ
        .find(_.isDefined)
        .flatten
        .getOrElse(sys.error("logic error"))

    def solve(): Long =
      val (parallelXY1, parallelXY2) = parallelInPlane(planeXY)
      println(s"parallelXY1=$parallelXY1, parallelXY2=$parallelXY2")
      val rockVelocityZ = rockVelocityInPlane(parallelXY1, parallelXY2, planeXZ, z => Vec(0, 0, z))
      println(s"rockVelocityZ=$rockVelocityZ")

      val (parallelXZ1, parallelXZ2) = parallelInPlane(planeXZ)
      println(s"parallelXZ1=$parallelXZ1, parallelXZ2=$parallelXZ2")
      val rockVelocityY = rockVelocityInPlane(parallelXZ1, parallelXZ2, planeYZ, y => Vec(0, y, 0))
      println(s"rockVelocityY=$rockVelocityY")

      val (parallelYZ1, parallelYZ2) = parallelInPlane(planeYZ)
      println(s"parallelYZ1=$parallelYZ1, parallelYZ2=$parallelYZ2")
      val rockVelocityX = rockVelocityInPlane(parallelYZ1, parallelYZ2, planeXZ, x => Vec(x, 0, 0))
      println(s"rockVelocityX=$rockVelocityX")

      val rockVelocity = rockVelocityX + rockVelocityY + rockVelocityZ
      println(s"rockVelocity=$rockVelocity")

      val collisionTime = collisionTimeRelativeToRockVelocity(rockVelocity)
      println(s"collisionTime=$collisionTime")

      val rockLocationBeforeCollision = rockVelocity * -collisionTime
      println(s"rockLocationBeforeCollision=$rockLocationBeforeCollision")

      ??? // giving up


  /** Attempt 2 - Didn't work
   * ------------------------------------------------------------------------------------------------------
   * Brute force for rock velocity, calculate the position for one stone, check whether all hit that spot.
   */
  object Attempt2:
    def solve(): Long =
      val stone0 = stones(0)
      val stone1 = stones(1)

      val rangeX = stones.map(_.velocity.x).min to stones.map(_.velocity.x).max
      val rangeY = stones.map(_.velocity.y).min to stones.map(_.velocity.y).max
      val rangeZ = stones.map(_.velocity.z).min to stones.map(_.velocity.z).max
      println(s"search space [${rangeZ.size}]")

      var vx: Long = rangeX.min
      var vy: Long = rangeY.min
      var vz: Long = rangeZ.min
      var found: Boolean = false
      var result: Option[(Long, Long, Long)] = None
      while !found do
        // simultaneous linear equation:
        // x = ab + av*t   y = bb + bv*t
        // x = cb + cv*u   y = db + dv*u
        val ab = stone0.location.x
        val av = stone0.velocity.x - vx
        val bb = stone0.location.y
        val bv = stone0.velocity.y - vy
        val cb = stone1.location.x
        val cv = stone1.velocity.x - vx
        val db = stone1.location.y
        val dv = stone1.velocity.y - vy
        val determinant = (av * dv) - (bv * cv)
        if determinant != 0 then
          val time = (dv * (cb - ab) - cv * (db - bb)) / determinant
          val x = stone0.location.x + stone0.velocity.x * time - vx * time
          val y = stone0.location.y + stone0.velocity.y * time - vy * time
          val z = stone0.location.z + stone0.location.z * time - vz * time
          // check if this rock throw will hit all hailstones
          found = stones.forall: stone =>
            val t =
              if      stone.velocity.x != vx then (x - stone.location.x) / (stone.velocity.x - vx)
              else if stone.velocity.y != vy then (y - stone.location.y) / (stone.velocity.y - vy)
              else if stone.velocity.z != vz then (y - stone.location.z) / (stone.velocity.z - vz)
              else sys.error("logic error")

            val hitsX = x + t * vx == stone.location.x + t * stone.velocity.x
            val hitsY = y + t * vy == stone.location.y + t * stone.velocity.y
            val hitsZ = z + t * vz == stone.location.z + t * stone.velocity.z
            hitsX && hitsY && hitsZ
          if found then
            result = Some(x, y, z)

        vx += 1
        if vx > rangeX.max then
          vx = rangeX.min
          vy += 1
        if vy > rangeY.max then
          vy = rangeY.min
          vz += 1
          println(s"countdown [${rangeZ.max - vz}]")
        if vz > rangeZ.max then
          sys.error("not found")

      result
        .map((x, y, z) => x + y + z)
        .getOrElse(sys.error("input error"))


  /** Attempt 3 - Finally
   * ------------------------------------------------------------------------------------------------------
   * Brute force for rock velocity re-framed to stand still by subtracting the rock velocity from the stone
   * velocity. Brute force for rock velocity on the XY plane, Calculate the Z velocity from two stones and
   * check whether the remaining ones hit that spot. Then calculate the time it took from the found stone
   * position, and calculate the unframed rock position backwards from its velocity and time.
   */
  object Attempt3:
    case class Pos(x: BigInt, y: BigInt, z: BigInt):
      def +(rhs: Pos): Pos = Pos(x + rhs.x, y + rhs.y, z + rhs.z)
      def -(rhs: Pos): Pos = Pos(x - rhs.x, y - rhs.y, z - rhs.z)
      def *(n: BigInt): Pos = Pos(x * n, y * n, z * n)

    case class Stone(position: Pos, velocity: Pos)

    def parse(input: String): Seq[Stone] =
      input
        .linesIterator
        .map:
          case s"$x, $y, $z @ $vx, $vy, $vz" =>
            Stone(
              position = Pos(x = x.trim.toLong, y = y.trim.toLong, z = z.trim.toLong),
              velocity = Pos(x = vx.trim.toLong, y = vy.trim.toLong, z = vz.trim.toLong))
        .toSeq

    /** straight forward future collide - this function overflows on input doubles thus we use big ints as positions */
    def futureCollide2D(l: Stone, r: Stone): Option[(Double, Double)] =

      // take the two lines but define them by two points each
      val Pos(x1, y1, _) = l.position
      val Pos(x2, y2, _) = l.position + l.velocity
      val Pos(x3, y3, _) = r.position
      val Pos(x4, y4, _) = r.position + r.velocity

      // inner cross product
      val denominator: BigInt =
        (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

      if denominator == 0 then
        // lines are parallel
        None
      else
        // intersection point - see wikipedia line intersection
        val x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)).doubleValue / denominator.toDouble
        val y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)).doubleValue / denominator.toDouble

        // does the intersection occur in both lines future
        val futureL = l.velocity.x.sign == (x - x1.doubleValue).sign && l.velocity.y.sign == (y - y1.doubleValue).sign
        val futureR = r.velocity.x.sign == (x - x3.doubleValue).sign && r.velocity.y.sign == (y - y3.doubleValue).sign
        Option.when(futureL && futureR)((x, y))

    extension (s: Stone) def reframe(velocity: Pos): Stone =
      s.copy(velocity = s.velocity - velocity)

    extension (ss: Seq[Stone]) def reframe(velocity: Pos): Seq[Stone] =
      ss.map(_.reframe(velocity))

    extension (d: Double) def toBigInt: BigInt =
      BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt

    extension (xy: (Double, Double)) def toBigInt: (BigInt, BigInt) =
      (xy._1.toBigInt, xy._2.toBigInt)

    def collide(stones: Seq[Stone]): Option[(BigInt, BigInt)] =
      futureCollide2D(stones(0), stones(1))
        .map(_.toBigInt)
        .flatMap: xy =>
          val hitall =
            stones
              .iterator
              .drop(2)
              .forall: stone =>
                futureCollide2D(stones(0), stone)
                  .map(_.toBigInt)
                  .contains(xy)
          Option.when(hitall)(xy)

    def calcT(stone: Stone, xy: (BigInt, BigInt)): BigInt =
      val (x, y) = xy
      if stone.velocity.x == 0 then (y - stone.position.y) / stone.velocity.y
      else (x - stone.position.x) / stone.velocity.x

    def calcZ(stones: Seq[Stone], xy: (BigInt, BigInt)): Option[BigInt] =

      def cross(l: Stone, r: Stone): BigInt =
        val timeL = calcT(l, xy)
        val timeR = calcT(r, xy)
        (l.position.z + timeL * l.velocity.z - (r.position.z + timeR * r.velocity.z)) / (timeL - timeR)

      val hit = cross(stones(0), stones(1))
      val hitAll = stones.iterator.drop(2).forall(stone => cross(stones(0), stone) == hit)
      Option.when(hitAll)(hit)

    def solve() =

      val stones: Seq[Stone] = parse(input)

      val search =
        for
          vx <- stones.map(_.velocity.x).min to stones.map(_.velocity.x).max
          vy <- stones.map(_.velocity.y).min to stones.map(_.velocity.y).max
        yield
          Pos(x = vx, y = vy, z = 0)

      val found: Pos =
        search
          .flatMap: velocity =>
            val translated = stones.reframe(velocity)
            collide(translated)
              .flatMap: location =>
                calcZ(translated, location).map((location, _))
              .map: (location, z) =>
                (location, velocity.copy(z = z))
              .map: (location, v) =>
                val z = calcT(translated(0), location) * (translated(0).velocity.z - v.z) + translated(0).position.z
                Pos(x = location._1, y = location._2, z = z)
          .head

      found.x + found.y + found.z


  case class Stone(location: Vec, velocity: Vec):

    def parallel(that: Stone, plane: Plane): Boolean =
      val (x1, y1) = plane.select(location)
      val (vx, vy) = plane.select(velocity)
      val x2 = x1 + vx
      val y2 = y1 + vy

      val (x3, y3) = plane.select(that.location)
      val (dx, dy) = plane.select(that.velocity)
      val x4 = x3 + dx
      val y4 = y3 + dy

      val determinant = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
      determinant == 0

    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    def futureIntersect(that: Stone, plane: Plane): Option[(Double, Double, Double)] =
      val (x1, y1) = plane.select(location)
      val (vx, vy) = plane.select(velocity)
      val x2 = x1 + vx
      val y2 = y1 + vy

      val (x3, y3) = plane.select(that.location)
      val (dx, dy) = plane.select(that.velocity)
      val x4 = x3 + dx
      val y4 = y3 + dy

      val determinant = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
      val time0 = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)).toDouble
      val time1 = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)).toDouble

      // https://math.stackexchange.com/questions/4038413/intersection-of-two-vectors-with-tails
      def future(time: Double): Boolean =
        (determinant >= 0 && time >= 0) || (determinant < 0 && time < 0)

      if determinant == 0 || !future(time0) || !future(time1) then
          None
      else
        val t = time0 / determinant
        val x = x1 + t * (x2 - x1)
        val y = y1 + t * (y2 - y1)
        Some((x, y, t))

  object Stone:
    def fromString(s: String): Stone =
      s match
        case s"$x, $y, $z @ $vx, $vy, $vz" =>
          Stone(Vec(x.trim.toLong, y.trim.toLong, z.trim.toLong), Vec(vx.trim.toLong, vy.trim.toLong, vz.trim.toLong))
        case _ => sys.error(s"input error: '$s'")

  case class Vec(x: Long, y: Long, z: Long):
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)
    def -(that: Vec): Vec = Vec(x - that.x, y - that.y, z - that.z)
    def *(that: Vec): Vec = Vec(x * that.x, y * that.y, z * that.z)
    def *(scalar: Long): Vec = Vec(x * scalar, y * scalar, z * scalar)
    def *(scalar: Double): Vec = Vec(x * scalar.toLong, y * scalar.toLong, z * scalar.toLong)
    def min(that: Vec): Vec = Vec(x min that.x, y min that.y, z min that.z)
    def max(that: Vec): Vec = Vec(x max that.x, y max that.y, z max that.z)
    def unary_- : Vec = Vec(-x, -y, -z)

  trait Plane:
    def select(v: Vec): (Long, Long)

  lazy val planeXY: Plane = v => (v.x, v.y)
  lazy val planeXZ: Plane = v => (v.x, v.z)
  lazy val planeYZ: Plane = v => (v.y, v.z)

  lazy val answer1: Int    = solve1(min = 200000000000000L, max = 400000000000000L)
  lazy val answer2: BigInt = Attempt3.solve()
