import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.math

object Day24 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val stones: Vector[Stone] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Stone.fromString)
      .toVector

  def solve1(min: Long, max: Long): Int =
    stones
      .combinations(2)
      .flatMap: combination =>
        combination(0).futureIntersect(combination(1), planeXY)
      .count: (x, y, _) =>
        x >= min && x <= max && y >= min && y <= max

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve1(min = 200000000000000L, max = 400000000000000L)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  /** Find two hailstones flying on parallel lines, two parallel lines define a plane in 3D space. Find two
   *  others, they define another plane, two planes intersecting define exactly one line which has to be the
   *  path the rock takes. Calculate intersection point for one hailstone + the line you just solved, find
   *  milliseconds for hail reaching said line from hailstone's initial position + velocity. Repeat for a
   *  different hailstone to get a different time, then work backwards from those times to figure out the
   *  velocity for your rock. After you have the velocity and the direction the rock is travelling on, you
   *  can iterate through all hailstones one last time to find the position needed on 0 milliseconds.
   */

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


  def solve2Intelligently_WhichIamNot(): Long =
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

    666

  def solve2BruteForce_WhichIamDefinitelyNot(): Long =
    val scope = 500
    val stone0 = stones(0)
    val stone1 = stones(1)

    println(s"building ")
    val search: IndexedSeq[Vec] =
      for
        vx <- -scope to scope
        vy <- -scope to scope
        vz <- -scope to scope
        if vx != 0 && vy != 0 && vz != 0
      yield
        Vec(vx, vy, vz)

    search
      .find: velocity =>
        // simultaneous linear equation:
        // x = ab + av*t   y = bb + bv*t
        // x = cb + cv*u   y = db + dv*u
        val ab = stone0.location.x
        val av = stone0.velocity.x - velocity.x
        val bb = stone0.location.y
        val bv = stone0.velocity.y - velocity.y
        val cb = stone1.location.x
        val cv = stone1.velocity.x - velocity.x
        val db = stone1.location.y
        val dv = stone1.velocity.y - velocity.y
        val determinant = (av * dv) - (bv * cv)
        if determinant != 0 then
          val time = (dv * (cb - ab) - cv * (db - bb)) / determinant
          val x = stone0.location.x + stone0.velocity.x * time - velocity.x * time
          val y = stone0.location.y + stone0.velocity.y * time - velocity.y * time
          val z = stone0.location.z + stone0.location.z * time - velocity.z * time
          // check if this rock throw will hit all hailstones
          stones.forall: stone =>
            val t =
              if      stone.velocity.x != velocity.x then (x - stone.location.x) / (stone.velocity.x - velocity.x)
              else if stone.velocity.y != velocity.y then (y - stone.location.y) / (stone.velocity.y - velocity.y)
              else if stone.velocity.z != velocity.z then (y - stone.location.z) / (stone.velocity.z - velocity.z)
              else sys.error("logic error")

            val hitsX = x + t * velocity.x == stone.location.x + t * stone.velocity.x
            val hitsY = y + t * velocity.y == stone.location.y + t * stone.velocity.y
            val hitsZ = z + t * velocity.z == stone.location.z + t * stone.velocity.z
            hitsX && hitsY && hitsZ
        else
          false
      .map: vec =>
        vec.x + vec.y + vec.z
      .getOrElse(sys.error("input error"))


  case class Pos(x: BigInt, y: BigInt, z: BigInt):
    def +(rhs: Pos): Pos = Pos(x + rhs.x, y + rhs.y, z + rhs.z)
    def -(rhs: Pos): Pos = Pos(x - rhs.x, y - rhs.y, z - rhs.z)
    def *(n: BigInt): Pos = Pos(x * n, y * n, z * n)

  case class Stone2(position: Pos, velocity: Pos)

  def parse(input: String): Seq[Stone2] =
    input
      .linesIterator
      .map:
        case s"$x, $y, $z @ $vx, $vy, $vz" =>
          Stone2(
            position = Pos(x = x.trim.toLong, y = y.trim.toLong, z = z.trim.toLong),
            velocity = Pos(x = vx.trim.toLong, y = vy.trim.toLong, z = vz.trim.toLong))
      .toSeq

  def futureIntersection2D(lhs: Stone2, rhs: Stone2): Option[(Double, Double)] =
    val Pos(x1, y1, _) = lhs.position
    val Pos(x2, y2, _) = lhs.position + lhs.velocity
    val Pos(x3, y3, _) = rhs.position
    val Pos(x4, y4, _) = rhs.position + rhs.velocity

    val denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    if denominator == 0 then None
    else
      val x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)).doubleValue / denominator.toDouble
      val y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)).doubleValue / denominator.toDouble
      val qqq = lhs.velocity.x.sign == (x - x1.doubleValue).sign && lhs.velocity.y.sign == (y - y1.doubleValue).sign
      val bla = rhs.velocity.x.sign == (x - x3.doubleValue).sign && rhs.velocity.y.sign == (y - y3.doubleValue).sign
      Option.when(qqq && bla)((x, y))

  def reframe(hail: Stone2, velocity: Pos): Stone2 =
    hail.copy(velocity = hail.velocity - velocity)

  def reframe(hail: Seq[Stone2], velocity: Pos): Seq[Stone2] =
    hail.map(reframe(_, velocity))

  def round(value: Double): BigInt =
    BigDecimal(value).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt

  def round(tupple: (Double, Double)): (BigInt, BigInt) =
    (round(tupple._1), round(tupple._2))

  def collide(stones: Seq[Stone2]): Option[(BigInt, BigInt)] =
    futureIntersection2D(stones(0), stones(1))
      .map(round)
      .flatMap: intersection =>
        val hitAll =
          stones
            .iterator
            .drop(2)
            .forall: stone =>
              futureIntersection2D(stones(0), stone)
                .map(round)
                .contains(intersection)
        Option.when(hitAll)(intersection)

  def calcT(hail: Stone2, intersection: (BigInt, BigInt)) =
    val (x, y) = intersection
    if hail.velocity.x == 0 then (y - hail.position.y) / hail.velocity.y
    else (x - hail.position.x) / hail.velocity.x

  def calcZ(stones: Seq[Stone2], intersection: (BigInt, BigInt)): Option[BigInt] =

    def cross(l: Stone2, r: Stone2): BigInt =
      val timeL = calcT(l, intersection)
      val timeR = calcT(r, intersection)
      (l.position.z + timeL * l.velocity.z - (r.position.z + timeR * r.velocity.z)) / (timeL - timeR)

    val hit = cross(stones(0), stones(1))
    val hitAll = stones.iterator.drop(2).forall(stone => cross(stones(0), stone) == hit)
    Option.when(hitAll)(hit)


  def solve2() =
    val input =
      Source
        .fromResource(s"input$day.txt")
        .mkString
        .trim

    val stones: Seq[Stone2] =
      parse(input)

    val search =
      for
        vx <- stones.map(_.velocity.x).min to stones.map(_.velocity.x).max
        vy <- stones.map(_.velocity.y).min to stones.map(_.velocity.y).max
      yield
        Pos(x = vx, y = vy, z = 0)

    val found: Pos =
      search
        .flatMap: velocity =>
          val translated = reframe(stones, velocity)
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










  val start2: Long    = System.currentTimeMillis
  val answer2: BigInt = solve2()
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

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


