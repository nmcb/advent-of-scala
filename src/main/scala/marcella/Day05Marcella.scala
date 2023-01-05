package marcella

import scala.io.*

object Day05Marcella extends App {

  val stacks = Stacks.from(
    Map(
      1 -> "RNFVLJSM",
      2 -> "PNDZFJWH",
      3 -> "WRCDG",
      4 -> "NBS",
      5 -> "MZWPCBFN",
      6 -> "PRMW",
      7 -> "RTNGLSW",
      8 -> "QTHFNBV",
      9 -> "LMHZNF"
    )
  )

  val actions: List[MoveAction] =
    Source
      .fromResource("input05.txt")
      .getLines
      .map(MoveAction.fromString)
      .toList

  val partOne = actions.foldLeft(stacks)(CrateMover9000.move).topline
  val partTwo = actions.foldLeft(stacks)(CrateMover9001.move).topline

  println(s"Answer to Part 1: $partOne")
  println(s"Answer to Part 2: $partTwo")
}

case class Stacks(under: Map[Int, Vector[Char]]):

  def topline: String =
    under                                                    // Map[Int, Vector[Char]]
      .toList                                                // List[(Int,Vector[Char])]
      .sortBy { case (stack, _) => stack }                   // List[(Int,Vector[Char])] - ascending on Int
      .flatMap { case (_, crates) => crates.lastOption }
      .mkString


  def getCrates(stackNumber: Int): Vector[Char] = under.getOrElse(stackNumber, Vector.empty)

  def addCrates(stackNumber: Int, crates: Vector[Char]): Stacks =
    val current = getCrates(stackNumber)
    copy(under.updated(stackNumber, current ++ crates))

  def removeCrates(stackNumber: Int, numCrates: Int): Stacks =
    copy(under.updatedWith(stackNumber)(_.map(_.dropRight(numCrates))))


object CrateMover9000 {
  def move(stacks: Stacks, action: MoveAction): Stacks =
    move(stacks, action.from, action.to, action.numCrates)

  private def move(stacks: Stacks, fromStack: Int, toStack: Int, numCrates: Int): Stacks = {
    val from = stacks.getCrates(fromStack)
    val crates = from.takeRight(numCrates).reverse

    stacks
      .removeCrates(fromStack, numCrates)
      .addCrates(toStack, crates)
  }
}

object CrateMover9001 {
  def move(stacks: Stacks, action: MoveAction): Stacks =
    move(stacks, action.from, action.to, action.numCrates)

  private def move(stacks: Stacks, fromStack: Int, toStack: Int, numCrates: Int): Stacks = {
    val from = stacks.getCrates(fromStack)
    val crates = from.takeRight(numCrates)

    stacks
      .removeCrates(fromStack, numCrates)
      .addCrates(toStack, crates)
  }
}

object Stacks {

  def from(map: Map[Int, String]): Stacks = {
    val stacks =
      map.map { case (stack, crates) => stack -> crates.toVector }
    Stacks(stacks)
  }
}

case class MoveAction(from: Int, to: Int, numCrates: Int)

object MoveAction:

  def fromString(str: String): MoveAction =
    str match
      case s"move $num from $from to $to" => MoveAction(from.toInt, to.toInt, num.toInt)