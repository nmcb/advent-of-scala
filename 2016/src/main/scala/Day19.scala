object Day19 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val input = 3017957

  /** @see [[https://youtu.be/uCsD3ZGzMgE]] */
  def solve1(input: Int): Int =
    val l = input - Integer.highestOneBit(input)
    l * 2 + 1

  val start1  = System.currentTimeMillis
  val answer1 = solve1(input)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** @see [[https://www.reddit.com/r/adventofcode/comments/5j4lp1/comment/dbdf50n/]] */
  def solve2(input: Int): Int =
    var i = 1
    while i * 3 < input do i *= 3
    input - i

  val start2  = System.currentTimeMillis
  val answer2 = solve2(input)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
