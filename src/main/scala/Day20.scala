import scala.collection.mutable
import scala.io.*
import scala.collection.mutable.*

object Day20 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val input: List[Long] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim.toLong)
      .toList

  def solve(input: List[Long], key: Int = 1, runs: Int = 1): List[Long] =

    extension (buffer: Buffer[(Long,Int)]) infix def mix(element: (Long,Int)): Buffer[(Long,Int)] =
      val (steps,_) = element
      val from = buffer.indexOf(element)
      buffer.remove(from)
      val to  = (from + steps) % buffer.size
      buffer.insert(if to < 0 then to.toInt + buffer.size else to.toInt, element)
      buffer

    val indexed: List[(Long, Int)] =
      input.map(_ * key).zipWithIndex

    val decrypted: List[(Long, Int)] =
      (0 until runs)
        .foldLeft(indexed.toBuffer)((acc,_) => indexed.foldLeft(acc)(_ mix _))
        .toList

    decrypted.map((v,_) => v)

  def grove(indices: List[Int])(coords: List[Long]): List[Long] =
    def get(i: Int): Long =
      val idx = (i + coords.indexOf(0)) % coords.length
      coords(idx)
    indices.map(get)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grove(List(1000,2000,3000))(solve(input)).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = grove(List(1000,2000,3000))(solve(input, 811589153, 10)).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
