import scala.annotation.tailrec
import scala.io.Source

object Day20 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    def move(direction: Char): Pos =
      direction match
        case 'N' => copy(y = y - 1)
        case 'E' => copy(x = x - 1)
        case 'S' => copy(y = y + 1)
        case 'W' => copy(x = x + 1)

  def solve(directions: String): Vector[Int] =

    type MaxDoorsTo = (Pos,Int)
    type Visited = Set[Pos]

    @tailrec
    def go(todo: String, maxDoorsTo: Map[Pos,Int], visited: Visited, branches: Vector[Visited]): Vector[Int] =
      todo.head match
        case '^' =>
          go(
            todo       = todo.tail,
            maxDoorsTo = Map(Pos(0,0) -> 0),
            visited    = Set(Pos(0,0)),
            branches   = Vector.empty
          )
        case '$' =>
          maxDoorsTo.values.toVector
        case '(' =>
          go(
            todo       = todo.tail,
            maxDoorsTo = maxDoorsTo,
            visited    = visited,
            branches   = Set.empty +: visited +: branches
          )
        case '|' =>
          val accumulated +: previous +: trail = branches: @unchecked
          go(
            todo       = todo.tail,
            maxDoorsTo = maxDoorsTo,
            visited    = previous,
            branches   = (accumulated ++ visited) +: previous +: trail
          )
        case ')' =>
          val accumulated +: previous +: trail = branches: @unchecked
          go(
            todo       = todo.tail,
            maxDoorsTo = maxDoorsTo,
            visited    = accumulated ++ visited,
            branches   = trail
          )
        case direction =>
          val (nextMaxDoorsTo, nextVisited) =
            visited.foldLeft((maxDoorsTo, Set.empty[Pos])):
              case ((maxDoorsTo, visited), currentRoom) =>
                val nextRoom = currentRoom.move(direction)
                if maxDoorsTo.contains(nextRoom) && maxDoorsTo(nextRoom) <= maxDoorsTo(currentRoom) + 1 then
                  (maxDoorsTo, visited + nextRoom)
                else
                  (maxDoorsTo.updated(nextRoom, maxDoorsTo(currentRoom) + 1), visited + nextRoom)
          go(
            todo       = todo.tail,
            maxDoorsTo = nextMaxDoorsTo,
            visited    = nextVisited,
            branches   = branches
          )
    go(
      todo        = directions,
      maxDoorsTo  = Map.empty,
      visited     = Set.empty,
      branches    = Vector.empty
    )


  val directions: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim

  val start1  = System.currentTimeMillis
  lazy val answer1 = solve(directions).max
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  lazy val answer2 = solve(directions).count(_ >= 1000)
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
