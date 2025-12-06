import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day15 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    infix def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)
    def neighbours: Set[Pos]    = Pos.adjacent.map(_ + this).toSet

  object Pos:
    val adjacent = Vector(Pos(1, 0), Pos(-1, 0), Pos(0, 1), Pos(0, -1))

  type Grid = Vector[Vector[Char]]

  extension (grid: Grid)
    def apply(p: Pos): Char = grid(p.y)(p.x)

  enum FighterType:
    case Elf, Goblin
    def target: FighterType =
      this match
        case Elf    => Goblin
        case Goblin => Elf

  import FighterType.*

  case class Fighter(fighterType: FighterType, pos: Pos, hitPoints: Int = 200, attackPower: Int = 3)

  given posReadingOrdering: Ordering[Pos]            = Ordering.by(p => (p.y, p.x))
  given combatUnitReadingOrdering: Ordering[Fighter] = Ordering.by(_.pos)

  def targetsOf(fighter: Fighter)(using fighters: List[Fighter]): Set[Fighter] =
    fighters.filter(_.fighterType == fighter.fighterType.target).toSet

  def isFree(pos: Pos)(using grid: Grid, fighters: List[Fighter]): Boolean =
    grid(pos) == '.' && !fighters.exists(_.pos == pos)

  def getInRange(targets: Set[Fighter])(using Grid, List[Fighter]): Set[Pos] =
    for
      target <- targets
      offset <- Pos.adjacent
      pos     = target.pos + offset
      if isFree(pos)
    yield
      pos

  type Distance = (Pos,Int)

  extension (distance: Distance)
    def target: Pos   = distance._1
    def distance: Int = distance._2

  /** breadth first search */
  def distances(from: Pos, to: Set[Pos])(using Grid, List[Fighter]): Map[Pos,Int] =
    @tailrec
    def go(visited: Map[Pos,Int], todo: Map[Pos,Int]): Map[Pos,Int] =
      val found = visited ++ todo
      todo.find((pos, _) => to.contains(pos)) match
        case Some(_) =>
          found
        case None =>
          val next =
            for
              (pos, dist) <- todo
              neighbour   <- pos.neighbours.filter(isFree).iterator
              if !visited.contains(neighbour)
            yield
              neighbour -> (dist + 1)
          if next.isEmpty then found else go(found, next)
    go(Map.empty, Map(from -> 0)).view.filterKeys(to).toMap

  def reachableBy(fighter: Fighter, inRange: Set[Pos])(using Grid, List[Fighter]): Map[Pos,Int] =
    distances(fighter.pos, inRange)

  def nearestBy(reachable: Map[Pos,Int]): Set[Pos] =
    val min = reachable.values.min
    reachable.filter(_.distance == min).keySet

  def choseBy(nearest: Set[Pos]): Pos =
    nearest.min

  def stepBy(chosen: Pos, fighter: Fighter)(using Grid, List[Fighter]): Pos =
    val neighbours = fighter.pos.neighbours
    val dists      = distances(chosen, neighbours)
    val min        = dists.values.min
    dists.filter(_.distance == min).keys.min

  class ElfDeathException extends RuntimeException

  def combat(grid: Grid, fighters: List[Fighter], elfDeath: Boolean = false): (Int,List[Fighter]) =

    def round(fighters: List[Fighter]): (List[Fighter], Boolean) =
      @tailrec
      def turn(todo: List[Fighter], done: List[Fighter], breakout: Boolean): (List[Fighter],Boolean) =
        done match
          case Nil =>
            (todo,breakout)
          case fighter :: rest =>
            given otherUnits: List[Fighter] = todo ++ rest
            given Grid                = grid

            val targets = targetsOf(fighter)
            val ready   = breakout || targets.isEmpty
            val inRange = getInRange(targets)

            var moved = fighter

            if !inRange.contains(fighter.pos) then
              val reachable = reachableBy(fighter, inRange)
              if reachable.nonEmpty then
                val nearest = nearestBy(reachable)
                val chosen  = choseBy(nearest)
                val step    = stepBy(chosen, fighter)
                moved       = fighter.copy(pos = step)


            val neighbours = moved.pos.neighbours
            val attackable = targetsOf(moved).filter(u => neighbours.contains(u.pos))

            if attackable.nonEmpty then
              val attackable1 = attackable.minBy(fighter => (fighter.hitPoints, fighter.pos))
              val attackable2 = attackable1.copy(hitPoints = attackable1.hitPoints - moved.attackPower)
              val attackable3 = Option.when(attackable2.hitPoints > 0)(attackable2)

              // Shut up! I know what I'm doing */
              if elfDeath && attackable3.isEmpty && attackable2.fighterType == Elf then
                throw new ElfDeathException

              val nextTodo = todo.flatMap(u => if u == attackable1 then attackable3 else Some(u))
              val nextRest = rest.flatMap(u => if u == attackable1 then attackable3 else Some(u))
              turn(moved :: nextTodo, nextRest, ready)
            else
              turn(moved :: todo, rest, ready)

      turn(Nil, fighters.sorted, breakout = false)

    type Search = (List[Fighter],Boolean)

    object Search:
      val init: Search = (fighters, false)
      val NotFound = false

    extension (search: Search)
      def result: List[Fighter] = search._1
      def found: Boolean        = search._2
      def next: Search          = round(result)

    type IndexedSearch = (Search,Int)

    extension (indexedSearch: IndexedSearch)
      def search: Search    = indexedSearch._1
      def index: Int        = indexedSearch._2
      def notFound: Boolean = !search.found


    val indexedSearch =
      Iterator
        .iterate(Search.init)(_.next)
        .zipWithIndex
        .dropWhile(_.notFound)
        .next

    (indexedSearch.index - 1, indexedSearch.search.result)

  def combatOutcome(grid: Grid, fighters: List[Fighter]): Int =
    val (rounds, survivors) = combat(grid, fighters)
    rounds * survivors.map(_.hitPoints).sum


  val (grid: Grid, fighters: List[Fighter]) =

    def parseGrid(input: String): Grid =
      input.linesIterator.map(_.toVector).toVector

    def parseFighter(tile: Char, pos: Pos): Option[Fighter] =
      val fighterType: Option[FighterType] =
        tile match
          case 'E' => Some(Elf)
          case 'G' => Some(Goblin)
          case _   => None
      fighterType.map(Fighter(_, pos))

    val input: String =
      Source.fromInputStream(getClass.getResourceAsStream(s"input$day.txt")).mkString.trim

    val fighters: List[Fighter] =
      for
        (row,y)  <- parseGrid(input).view.zipWithIndex.toList
        (tile,x) <- row.view.zipWithIndex
        fighter  <- parseFighter(tile, Pos(x,y))
      yield fighter

    val grid: Grid =
      parseGrid(input.replace('E', '.').replace('G', '.'))

    (grid, fighters)

  val start1  = System.currentTimeMillis
  lazy val answer1 = combatOutcome(grid, fighters)
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def combatOutcomeElfWin(grid: Grid, fighters: List[Fighter]): Int =
    
    def withElfAttackPower(elfAttackPower: Int): Option[Int] =
      val updated = fighters.map:
        case elf @ Fighter(Elf, _, _, _) => elf.copy(attackPower = elfAttackPower)
        case fighter                     => fighter

      Try(combat(grid, updated, elfDeath = true)).toOption
        .map: (rounds, fighters) =>
          rounds * fighters.map(_.hitPoints).sum

    (4 to 200)
      .groupMapReduce(e => math.ceil(200.0 / e).toInt)(identity)(_ min _)
      .values.toList.sorted
      .flatMap(withElfAttackPower)
      .head

  val start2  = System.currentTimeMillis
  lazy val answer2 = combatOutcomeElfWin(grid, fighters)
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
