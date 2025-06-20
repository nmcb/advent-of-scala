object Day21 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit).mkString

  case class Dice(min: Int, max: Int, cur: Int = 0, rolled: Int = 0):

    def face: Int =
      cur

    def roll: Dice =
      if cur >= max then
        copy(cur = min, rolled = rolled + 1)
      else
        copy(cur = cur + 1, rolled = rolled + 1)

  case class Player(name: String, start: Int, rolled: Seq[Int] = Seq.empty, score: Int = 0)

  case class Game1(player1: Player, player2: Player, pawn1: Int, pawn2: Int, dice: Dice, goal: Int)

  def solve1(g: Game1): Int =

    def won(p: Player): Boolean =
      p.score >= g.goal

    if won(g.player2) then
      g.dice.rolled * g.player1.score
    else
      val throw1 = g.dice.roll
      val throw2 = throw1.roll
      val throw3 = throw2.roll

      val position = 1 + ((g.pawn1 - 1 + throw1.face + throw2.face + throw3.face) % 10)
      val played   = g.player1.copy(score = g.player1.score + position)
      solve1(
        g.copy(
          player1 = g.player2,
          player2 = played,
          dice = throw3,
          pawn1 = g.pawn2,
          pawn2 = position
        )
      )


  val player1 = Player(name = "#1", start = 7)
  val player2 = Player(name = "#2", start = 9)
  val game1   = Game1(player1, player2, player1.start, player2.start, Dice(1, 100), 1000)


  val start1  = System.currentTimeMillis
  val answer1 = solve1(game1)
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  case class Pawn(pos: Int, score: Int = 0):

    def move(steps: Int): Pawn =
      val npos = ((pos - 1 + steps) % 10) + 1
      Pawn(npos, score + npos)

  val roll3x3x3 =
    val throws =
      for
        t1 <- 1 to 3
        t2 <- 1 to 3
        t3 <- 1 to 3
      yield t1 + t2 + t3

    throws.groupMapReduce(identity)(_ => 1)(_ + _)

  def solve2(pawn1: Pawn, pawn2: Pawn): Long =

    def go(pawn1: Pawn, pawn2: Pawn): (Long, Long) =

      def getOrTurn(pawn1: Pawn, pawn2: Pawn): (Long, Long) =
        if pawn1.score >= 21 then (1, 0) else go(pawn2, pawn1).swap

      val turns =
        for
          (roll, count) <- roll3x3x3
          npawn1         = pawn1.move(roll)
          (u1, u2)       = getOrTurn(npawn1, pawn2)
        yield (count * u1, count * u2)

      turns.reduce:
        case ((u1a, u2a), (u1b, u2b)) => (u1a + u1b, u2a + u2b)

    val (score1, score2) = go(pawn1, pawn2)
    score1 max score2


  val start2 = System.currentTimeMillis
  val answer2 = solve2(Pawn(pos = 7), Pawn(pos = 9))
  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
