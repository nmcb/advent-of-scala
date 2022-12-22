import scala.io.*
import Robot.*
import Material.*

enum Material:
  case Ore
  case Clay
  case Obsidian
  case Geode

case class Robot(material: Material)

case class Blueprint( index: Int, oreRobotCost: Int, clayRobotCost: Int, obsidianRobotCostOre: Int, obsidianRobotCostClay: Int, geodeRobotCostOre: Int, geodeRobotCostObsidian: Int):
  val highestOreCost = List(oreRobotCost, clayRobotCost, obsidianRobotCostOre, geodeRobotCostOre).max

case class MaterialStash(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0)

sealed trait Action
case object DoNothing extends Action
case object BuildOreRobot extends Action
case object BuildClayRobot extends Action
case object BuildObsidianRobot extends Action
case object BuildGeodeRobot extends Action

def parseBlueprint(s: String): Blueprint =
  s match {
    case s"Blueprint ${index}: Each ore robot costs ${oreRobotCost} ore. Each clay robot costs ${clayRobotCost} ore. Each obsidian robot costs ${obsidianRobotCostOre} ore and ${obsidianRobotCostClay} clay. Each geode robot costs ${geodeRobotCostOre} ore and ${geodeRobotCostObsidian} obsidian." => Blueprint(index.toInt, oreRobotCost.toInt, clayRobotCost.toInt, obsidianRobotCostOre.toInt, obsidianRobotCostClay.toInt, geodeRobotCostOre.toInt, geodeRobotCostObsidian.toInt)
    case _ => sys.error("booboo")
  }

def checkBuildRobot(blueprint: Blueprint, material: MaterialStash, robots: List[Robot]): List[Action] =
  val curOre = material.ore
  val curClay = material.clay
  val curObsidian = material.obsidian

  def count(robot: Robot): Int =
    robots.count(_ == robot)

  val oreRobot: Option[Action] =
    if curOre >= blueprint.oreRobotCost && count(Robot(Ore)) <= blueprint.highestOreCost then
      Some(BuildOreRobot)
    else
      None
  val clayRobot: Option[Action] =
    if curOre >= blueprint.clayRobotCost && count(Robot(Clay)) <= blueprint.obsidianRobotCostClay then
      Some(BuildClayRobot)
    else
      None
  val obsidianRobot: Option[Action] =
    if curOre >= blueprint.obsidianRobotCostOre && curClay >= blueprint.obsidianRobotCostClay then
      Some(BuildObsidianRobot)
    else
      None
  val geodeRobot: Option[Action] =
    if curOre >= blueprint.geodeRobotCostOre && curObsidian >= blueprint.geodeRobotCostObsidian then
      Some(BuildGeodeRobot)
    else
      None

  if material.obsidian >= blueprint.geodeRobotCostObsidian then
    List(geodeRobot).flatten
  else if material.clay >= blueprint.obsidianRobotCostClay then
    List(geodeRobot, obsidianRobot, Some(DoNothing)).flatten
  else if material.ore >= blueprint.highestOreCost then
    List(oreRobot, clayRobot, obsidianRobot, geodeRobot).flatten
  else
    List(oreRobot, clayRobot, obsidianRobot, geodeRobot, Some(DoNothing)).flatten


def buildRobot(action: Action): Robot =
  action match {
    case BuildOreRobot      => Robot(Ore)
    case BuildClayRobot     => Robot(Clay)
    case BuildObsidianRobot => Robot(Obsidian)
    case BuildGeodeRobot    => Robot(Geode)
    case _ => sys.error("boom robot!")
  }

def produceMaterial(robots: List[Robot], stash: MaterialStash): MaterialStash = {
  robots match {
    case h :: t => h match {
      case Robot(Ore)       => produceMaterial(t, stash.copy(ore = stash.ore + 1))
      case Robot(Clay)      => produceMaterial(t, stash.copy(clay = stash.clay + 1))
      case Robot(Obsidian)  => produceMaterial(t, stash.copy(obsidian = stash.obsidian + 1))
      case Robot(Geode)     => produceMaterial(t, stash.copy(geode = stash.geode + 1))
    }
    case Nil => stash
  }
}

def consumeMaterial(action: Action, blueprint: Blueprint, stash: MaterialStash): MaterialStash = {
  action match {
    case BuildOreRobot      => stash.copy(ore = stash.ore - blueprint.oreRobotCost)
    case BuildClayRobot     => stash.copy(ore = stash.ore - blueprint.clayRobotCost)
    case BuildObsidianRobot => stash.copy(ore = stash.ore - blueprint.obsidianRobotCostOre, clay = stash.clay - blueprint.obsidianRobotCostClay)
    case BuildGeodeRobot    => stash.copy(ore = stash.ore - blueprint.geodeRobotCostOre, obsidian = stash.obsidian - blueprint.geodeRobotCostObsidian)
    case _ => sys.error("boom robot!")
  }
}

def geodeCollectionFlows(blueprint: Blueprint, material: MaterialStash, robots: List[Robot], timeLimit: Int, curMax: Int, currentTime: Int = 1): Int =

  val newMax =
    if currentTime == timeLimit && material.geode + robots.count(_ == Robot(Geode)) > curMax then
      material.geode + robots.count(_ == Robot(Geode))
    else
      curMax

  if currentTime == timeLimit then
    material.geode + robots.count(_ == Robot(Geode))
  else
    val newActions = checkBuildRobot(blueprint, material, robots)
    newActions.foldLeft(newMax)((max, action) =>
      if action == DoNothing then
        val newMaterial = produceMaterial(robots, material)
        val possibleScore = geodeCollectionFlows(blueprint, newMaterial, robots, timeLimit, newMax, currentTime + 1)
        if (possibleScore > max) {possibleScore} else max
      else
        val newRobot = buildRobot(action)
        val newMaterial = consumeMaterial(action, blueprint, produceMaterial(robots, material))
        val possibleScore = geodeCollectionFlows(blueprint, newMaterial, newRobot :: robots, timeLimit, newMax, currentTime + 1)
        if (possibleScore > max) {possibleScore} else max
    )

object Day19 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val input =
    Source.fromResource(s"input$day.txt").getLines.map(parseBlueprint).toList

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    input
      .map(x =>
        geodeCollectionFlows(x, MaterialStash(), List(Robot(Ore)), 24, 0) * x.index
      ).sum

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    input
      .take(3)
      .map(x =>
        geodeCollectionFlows(x, MaterialStash(), List(Robot(Ore)), 32, 0)
      ).product

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
