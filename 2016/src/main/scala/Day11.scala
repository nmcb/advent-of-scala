import scala.collection.*
import scala.io.Source

object Day11 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Floor(microchips: Int, generators: Int):
    def empty: Boolean = microchips == 0 && generators == 0
    def valid: Boolean = (microchips >= 0 && generators >= 0) && (generators == 0 || microchips <= generators)

    def +(that: Floor): Floor = Floor(microchips + that.microchips, generators + that.generators)
    def -(that: Floor): Floor = Floor(microchips - that.microchips, generators - that.generators)

  /** hardcoded for 4 floors */
  case class Area(elevator: Int, floors: Vector[Floor]):
    def finished: Boolean = elevator == 3 && floors.take(3).forall(_.empty)
    def valid: Boolean    = floors.forall(_.valid)

    def candidates: Vector[Area] =
      for
        adjustment  <- Area.elevatorAdjustments
        destination <- Area.adjacentFloors(elevator)
      yield
        Area(
          elevator = destination,
          floors   = floors
                       .updated(elevator, floors(elevator) - adjustment)
                       .updated(destination, floors(destination) + adjustment)
        )

  object Area:
    val elevatorAdjustments = Vector(Floor(2, 0), Floor(1, 0), Floor(1, 1), Floor(0, 1), Floor(0, 2))
    val adjacentFloors      = Map(0 -> Vector(1), 1 -> Vector(0, 2), 2 -> Vector(1, 3), 3 -> Vector(2))


  lazy val init: Area =
    val lines =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector
    val microchip = "microchip".r
    val generator = "generator".r
    Area(0, lines.map(line => Floor(microchip.findAllIn(line).size, generator.findAllIn(line).size)))


  /** breadth first search */
  def solve(start: Area): Int =
    val todo  = mutable.Queue(start)
    val cache = mutable.Map(start -> 0)

    while todo.nonEmpty do
      val current = todo.dequeue
      val cost    = cache(current) + 1
      current.candidates.filter(_.valid).foreach: next =>
        if !cache.contains(next) || cost < cache(next) then
          cache(next) = cost
          todo.enqueue(next)

    cache(cache.keys.filter(_.finished).head)


  val start1  = System.currentTimeMillis
  val answer1 = solve(start = init)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val expanded: Area =
    val floor0 = init.floors(0) + Floor(generators = 2, microchips = 2)
    init.copy(floors = init.floors.updated(0, floor0))

  val start2  = System.currentTimeMillis
  val answer2 = solve(start = expanded)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
