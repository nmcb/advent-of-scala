import scala.collection.*

object Day22 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  enum RegionType(val riskLevel: Int):
    case Rocky  extends RegionType(0)
    case Wet    extends RegionType(1)
    case Narrow extends RegionType(2)

  case class Region(x: Int, y: Int):
    infix def +(x: Int, y: Int): Region    = copy(x = this.x + x, y = this.y + y)
    infix def manhattan(that: Region): Int = (x - that.x).abs + (y - that.y).abs

    def geologicalIndex: Int      = Region.geologicalIndex(this)
    def erosionLevel: Int         = Region.erosionLevel(this)
    def regionType: RegionType    = RegionType.fromOrdinal(erosionLevel % 3)
    def neighbouring: Set[Region] = Set((-1,0),(1,0),(0,-1),(0,1)).map(this.+)


  object Region:

    val depth: Int     = 11817
    val mouth: Region  = Region(0, 0)
    val target: Region = Region(x = 9, y = 751)

    private val geologicalIndices = mutable.Map.empty[Region,Int]
    private def geologicalIndex(r: Region): Int =
      geologicalIndices.getOrElseUpdate(r,
        r match
          case Region.mouth  => 0
          case Region.target => 0
          case Region(x, 0)  => x * 16807
          case Region(0, y)  => y * 48271
          case _             => r.copy(x = r.x - 1).erosionLevel * r.copy(y = r.y - 1).erosionLevel
      )

    private val erosionLevels = mutable.Map.empty[Region,Int]
    private def erosionLevel(r: Region): Int =
      erosionLevels.getOrElseUpdate(r,
        (r.geologicalIndex + depth) % 20183
      )

  def sumOfRiskLevels(min: Region, max: Region): Int =
    var result = 0
    for y <- min.y to max.y ; x <- min.x to max.x do result = result + Region(x,y).regionType.riskLevel
    result


  val start1  = System.currentTimeMillis
  val answer1 = sumOfRiskLevels(Region.mouth, Region.target)
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  enum Tool:
    case None
    case Torch
    case ClimbingGear

  import RegionType.*
  import Tool.*

  extension (regionType: RegionType)

    def possibleTools: Set[Tool] =
      regionType match
        case Rocky  => Set(ClimbingGear, Torch)
        case Wet    => Set(ClimbingGear, None)
        case Narrow => Set(Torch, None)

  type State = (Region,Tool)

  object State:
    val initial: State = (Region.mouth, Torch)
    val target: State  = (Region.target, Torch)
    val toolSwitchingTime = 7


  extension (state: State)
    def region: Region = state._1
    def equipped: Tool = state._2

    def actions: Map[State,Int] =
      val moves =
        region
          .neighbouring
          .filter(neighbour => neighbour.x >= 0 && neighbour.y >= 0)
          .filter(_.regionType.possibleTools.contains(equipped))
          .map((_, equipped) -> 1)

      val changes =
        region
          .regionType
          .possibleTools
          .filterNot(_ == equipped)
          .map((region, _) -> 7)

      (moves ++ changes).toMap


  /** a-star with manhattan distance as heuristic */
  def travelTime(from: State, to: State): Int =
    val times = collection.mutable.Map(from -> 0)
    val queue = collection.mutable.PriorityQueue(from -> 0)(using Ordering.by[(State,Int),Int](_._2).reverse)

    def heuristic(state: State) =
      state.region manhattan to.region

    while queue.nonEmpty do
      val (current, _) = queue.dequeue()

      if current == to then return times(to)

      current.actions.foreach: (neighbour,delta) =>
        val total = times(current) + delta
        if !times.contains(neighbour) || total < times(neighbour) then
          times(neighbour) = total
          val priority = total + heuristic(neighbour)
          queue.enqueue(neighbour -> priority)
    -1

  val start2  = System.currentTimeMillis
  val answer2 = travelTime(State.initial, State.target)
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
