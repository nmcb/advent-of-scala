package nmcb

abstract class AoC:

  val day: String = getClass.getName.replace('.', '/').init

  val answer1: Any
  val answer2: Any

  def main(args: Array[String]): Unit =
    val start1: Long = System.currentTimeMillis
    println(s"Answer $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

    val start2: Long = System.currentTimeMillis
    println(s"Answer $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

