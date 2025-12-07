package aoc2022

import nmcb.*

import scala.annotation.tailrec

object Day19 extends AoC:

  val blueprints: Vector[Blueprint] = lines.map(Blueprint.fromString).toVector

  case class Blueprint(
    index:                  Int,
    oreRobotCost:           Int,
    clayRobotCost:          Int,
    obsidianRobotCostOre:   Int,
    obsidianRobotCostClay:  Int,
    geodeRobotCostOre:      Int,
    geodeRobotCostObsidian: Int
  ):
    val maxRobotCostOre: Int =
      Vector(oreRobotCost, clayRobotCost, obsidianRobotCostOre, geodeRobotCostOre).max

  object Blueprint:
    def fromString(s: String): Blueprint =
      s match
        case s"""Blueprint ${nr}: Each ore robot costs ${c1} ore. Each clay robot costs ${c2} ore. Each obsidian robot costs ${c3} ore and ${c4} clay. Each geode robot costs ${c5} ore and ${c6} obsidian.""" =>
          Blueprint(nr.toInt, c1.toInt, c2.toInt, c3.toInt, c4.toInt, c5.toInt, c6.toInt)

  enum Material:
    case Ore
    case Clay
    case Obsidian
    case Geode

  import Material.*

  case class Robot(material: Material)

  case class MaterialStash(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0)

  enum Build:
    case Nothing
    case OreRobot
    case ClayRobot
    case ObsidianRobot
    case GeodeRobot

  import Build.*

  def buildActions(print: Blueprint, material: MaterialStash, robots: Vector[Robot]): Vector[Build] =
    import material.*
    import print.*

    def robotCount(material: Material): Int =
      robots.count(_ == Robot(material))

    import Option.*

    val buildOre      = when(ore >= oreRobotCost         && robotCount(Ore)  <= maxRobotCostOre)(OreRobot)
    val buildClay     = when(ore >= clayRobotCost        && robotCount(Clay) <= obsidianRobotCostClay)(ClayRobot)
    val buildObsidian = when(ore >= obsidianRobotCostOre && clay             >= obsidianRobotCostClay)(ObsidianRobot)
    val buildGeode    = when(ore >= geodeRobotCostOre    && obsidian         >= geodeRobotCostObsidian)(GeodeRobot)

    val actions: Vector[Option[Build]] =
      if      obsidian >= geodeRobotCostObsidian then Vector(buildGeode, Some(Nothing))
      else if clay     >= obsidianRobotCostClay  then Vector(buildGeode, buildObsidian, Some(Nothing))
      else if ore      >= maxRobotCostOre        then Vector(buildOre, buildClay, buildObsidian, buildGeode)
      else                                            Vector(buildOre, buildClay, buildObsidian, buildGeode, Some(Nothing))

    actions.flatten

  def build(action: Build): Robot =
    action match
      case OreRobot      => Robot(Ore)
      case ClayRobot     => Robot(Clay)
      case ObsidianRobot => Robot(Obsidian)
      case GeodeRobot    => Robot(Geode)
      case Nothing       => sys.error("boom robot!")

  @tailrec
  def produce(robots: Vector[Robot], stash: MaterialStash): MaterialStash =
    import stash.*
    robots match
      case h +: t =>
        h match
          case Robot(Ore)      => produce(t, stash.copy(ore = ore + 1))
          case Robot(Clay)     => produce(t, stash.copy(clay = clay + 1))
          case Robot(Obsidian) => produce(t, stash.copy(obsidian = obsidian + 1))
          case Robot(Geode)    => produce(t, stash.copy(geode = geode + 1))
      case Vector() =>
        stash
      case _ =>
        sys.error(s"invalid state robots=$robots")

  def consume(build: Build, print: Blueprint, stash: MaterialStash): MaterialStash =
    import print.*
    import stash.*
    build match
      case OreRobot      => stash.copy(ore = ore - oreRobotCost)
      case ClayRobot     => stash.copy(ore = ore - clayRobotCost)
      case ObsidianRobot => stash.copy(ore = ore - obsidianRobotCostOre, clay = clay - obsidianRobotCostClay)
      case GeodeRobot    => stash.copy(ore = ore - geodeRobotCostOre, obsidian = obsidian - geodeRobotCostObsidian)
      case Nothing       => stash

  def simulate(
    blueprint: Blueprint,
    timeLimit: Int,
    material: MaterialStash = MaterialStash(),
    robots: Vector[Robot]   = Vector(Robot(Ore)),
    max: Int                = 0,
    time: Int               = 1
  ): Int =

    val updated: Int = material.geode + robots.count(_ == Robot(Geode))
    val next: Int = if time == timeLimit && updated > max then updated else max

    if time == timeLimit then updated else
      buildActions(blueprint, material, robots).foldLeft(next): (score,action) =>
        action match
          case Nothing =>
            val produced = produce(robots, material)
            val scored   = simulate(blueprint, timeLimit, produced, robots, next, time + 1)
            if scored > score then scored else score
          case _ =>
            val builds   = build(action)
            val consumed = consume(action, blueprint, produce(robots, material))
            val scored   = simulate(blueprint,timeLimit, consumed, builds +: robots, next, time + 1)
            if scored > score then scored else score


  lazy val answer1: Int = blueprints.map(print => simulate(print, 24) * print.index).sum
  lazy val answer2: Int = blueprints.take(3).map(print => simulate(print, 32)).product
