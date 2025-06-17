import scala.io._

object Day06 extends App:
  val start = System.currentTimeMillis

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val ages: Map[Int,Long] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .groupMapReduce(_.toInt)(_ => 1L)(_ + _)

  val FirstSpawnAfter = 8
  val SpawnAfter      = 6
  val SpawnNow        = -1

  extension (generation: Map[Int,Long])
    def next: Map[Int,Long] =
      val aged    = generation.map((timer,count) => (timer - 1) -> count)
      val spawned = aged.getOrElse(SpawnNow, 0L)
      val next    = aged.getOrElse(SpawnAfter, 0L) + spawned
      aged.removed(SpawnNow) + (FirstSpawnAfter -> spawned) + (SpawnAfter -> next)


  def solve(generations: Map[Int,Long], years: Int): Long =
    Iterator.iterate(generations)(_.next).drop(years).next.values.sum

  val start1  = System.currentTimeMillis
  val answer1 = solve(ages, 80)
  println(s"Day $day answer = ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  val answer2 = solve(ages, 256)
  println(s"Day $day answer = ${answer2} [${System.currentTimeMillis - start2}ms]")
