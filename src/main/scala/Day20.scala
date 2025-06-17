object Day20 extends App:

  import scala.io.*

  val day: String =
    this.getClass.getName.drop(3).init

  /** Input */

  val input: Int =
    36000000

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  val answer1 =
    val presents: Array[Int] = Array.fill(input / 10 + 1)(0) ; for {
      e <- Range.inclusive(1, input / 10)
      h <- Range.inclusive(e, input / 10, e)
    } yield presents(h) = presents(h) + e * 10

    presents
      .zipWithIndex
      .find((count,_) => count >= input)
      .getOrElse(sys.error("not found"))
      ._2

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    val size = input / 10 + 1
    val presents: Array[Int] = Array.fill(size)(0) ; for {
      e <- Range.inclusive(1, size)
      h <- Range.inclusive(e, e * 50, e)
      if h < size
    } yield presents(h) = presents(h) + e * 11

    presents
      .zipWithIndex
      .find((count,_) => count >= input)
      .getOrElse(sys.error("not found"))
      ._2

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
