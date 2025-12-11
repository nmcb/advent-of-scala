package aoc2025

import nmcb.*
import scala.collection.mutable


object Day11 extends AoC:

  lazy val edges: Map[String, Vector[String]] =
    lines
      .collect:
        case s"$from: $toes" => from -> toes.split(" ").toVector
      .toMap
      .withDefaultValue(Vector.empty)

  def solve1(from: String, to: String, edges: Map[String, Vector[String]]): Long =
    val cache = mutable.Map.empty[String, Long]
    def loop(from: String): Long = cache.getOrElseUpdate(from,
      if from == to then 1 else edges(from).map(loop).sum
    )
    loop(from)

  def solve2(edges: Map[String,Vector[String]]): Long =

    def paths(chain: Vector[String]): Long =
      chain.sliding(2).map(ns => solve1(ns.head, ns.tail.head, edges)).product

    Vector(
      Vector("svr", "dac", "fft", "out"),
      Vector("svr", "fft", "dac", "out")
    ).map(paths).sum
  
  lazy val answer1: Long = solve1("you", "out", edges)
  lazy val answer2: Long = solve2(edges)
