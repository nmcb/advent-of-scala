object Day15 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  def next(factor: Long): Long => Long =
    (current: Long) => current * factor % 2147483647L

  def same(a: Long, b: Long): Boolean =
    (a & 0xffff) == (b & 0xffff)

  def generatorA = Iterator.iterate(591L)(next(16807)).drop(1)
  def generatorB = Iterator.iterate(393L)(next(48271)).drop(1)
  val generator1 = generatorA zip generatorB
  val generator2 = generatorA.filter(_ % 4 == 0) zip generatorB.filter(_ % 8 == 0)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = generator1.take(40000000).count(same)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = generator2.take(5000000).count(same)
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
