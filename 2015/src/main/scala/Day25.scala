object Day25 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  def indexOf(row: Int, column: Int): Int =
    val diagonal = row + column - 2
    val triangle = (1 to diagonal).sum
    triangle + column

  val start1   = System.currentTimeMillis

  val initial  = BigInt(20151125)
  val base     = BigInt(252533)
  val modulo   = BigInt(33554393)
  val exponent = indexOf(row = 2947, column = 3029) - 1
  val answer1  = initial * base.modPow(exponent, modulo) % modulo

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")
