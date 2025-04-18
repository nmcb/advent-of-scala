import scala.io.Source

object Day20 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  case class Vec(x: Int, y: Int,z: Int):

    infix def +(that: Vec): Vec =
      Vec(x + that.x, y + that.y, z + that.z)

    infix def distance: Int =
      x.abs + y.abs + z.abs

  case class Particle(p: Vec, v: Vec, a: Vec):

    def update: Particle =
      copy(p = p + v + a, v = v + a)

  extension [A](t: (A,Int))
    def element: A = t._1
    def index: Int = t._2

  extension (particles: Vector[Particle])

    def tick: Vector[Particle] =
      particles.map(_.update)

    def minDistanceParticleIndex: Int =
      particles
        .zipWithIndex
        .minBy((particle,_) => particle.p.distance)
        .index

    def tickWithCollisions: Vector[Particle] =
      particles
        .map(_.update)
        .groupBy(_.p)
        .filter((_, collided) => collided.size == 1)
        .values
        .flatten
        .toVector

  private val particles: Vector[Particle] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"p=<$px,$py,$pz>, v=<$vx,$vy,$vz>, a=<$ax,$ay,$az>" =>
          val p = Vec(px.toInt, py.toInt, pz.toInt)
          val v = Vec(vx.toInt, vy.toInt, vz.toInt)
          val a = Vec(ax.toInt, ay.toInt, az.toInt)
          Particle(p, v, a)
      .toVector

  val start1: Long =
    System.currentTimeMillis

  /** assume 500 simulation iterations to be enough */
  val answer1: Int =
    Iterator
      .iterate(particles)(_.tick)
      .map(minDistanceParticleIndex)
      .take(500)
      .toList
      .last

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  /** assume 100 simulation iterations to be enough */
  val answer2: Int =
    Iterator
      .iterate(particles)(tickWithCollisions)
      .take(100)
      .toList
      .last
      .size

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
