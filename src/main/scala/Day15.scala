object Day15 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  def next(factor: Long): Long => Long =
    (current: Long) => current * factor % 2147483647L

  def same(a: Long, b: Long): Boolean =
    (a & 0xffff) == (b & 0xffff)

  val generatorA1 = Iterator.iterate(591L)(next(16807)).drop(1)
  val generatorB1 = Iterator.iterate(393L)(next(48271)).drop(1)
  val pairs1     = generatorA1.zip(generatorB1)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = pairs1.take(40000000).count(same)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val generatorA2 = Iterator.iterate(591L)(next(16807)).drop(1)
  val generatorB2 = Iterator.iterate(393L)(next(48271)).drop(1)
  val pairs2 = generatorA2.filter(_ % 4 == 0).zip(generatorB2.filter(_ % 8 == 0))

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = pairs2.take(5000000).count(same)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
