import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(69501)(Day01.answer1)
    assertResult(202346)(Day01.answer2)
  
  test("Day02"):
    assertResult(11449)(Day02.answer1)
    assertResult(13187)(Day02.answer2)
  
  test("Day03"):
    assertResult(8139)(Day03.answer1)
    assertResult(2668)(Day03.answer2)
  
  test("Day04"):
    assertResult(651)(Day04.answer1)
    assertResult(956)(Day04.answer2)
  
  test("Day05"):
    assertResult("WHTLRMZRC")(Day05.answer1)
    assertResult("GMPMLWNMG")(Day05.answer2)
  
  test("Day06"):
    assertResult(1093)(Day06.answer1)
    assertResult(3534)(Day06.answer2)
  
  test("Day07"):
    assertResult(1915606)(Day07.answer1)
    assertResult(5025657)(Day07.answer2)

  test("Day08"):
    assertResult(1818)(Day08.answer1)
    assertResult(368368)(Day08.answer2)

  test("Day09"):
    assertResult(6376)(Day09.answer1)
    assertResult(2607)(Day09.answer2)

  test("Day10"):
    assertResult(12520)(Day10.answer1)
    assertResult(
      """####.#..#.###..####.###....##..##..#....
        |#....#..#.#..#....#.#..#....#.#..#.#....
        |###..####.#..#...#..#..#....#.#....#....
        |#....#..#.###...#...###.....#.#.##.#....
        |#....#..#.#....#....#....#..#.#..#.#....
        |####.#..#.#....####.#.....##...###.####.
        |""".stripMargin.trim)(Day10.answer2)

  test("Day11"):
    assertResult(98280)(Day11.answer1)
    assertResult(17673687232L)(Day11.answer2)

  test("Day12"):
    assertResult(383)(Day12.answer1)
    assertResult(377)(Day12.answer2)

  test("Day13"):
    assertResult(5760)(Day13.answer1)
    assertResult(26670)(Day13.answer2)

  test("Day14"):
    assertResult(793)(Day14.answer1)
    assertResult(24166)(Day14.answer2)

  test("Day15"):
    assertResult(5870801)(Day15.answer1)
    assertResult(10908230916597L)(Day15.answer2)

  test("Day16"):
    assertResult(2265)(Day16.answer1)
    assertResult(2811)(Day16.answer2)

  test("Day17"):
    assertResult(3130 )(Day17.answer1)
    assertResult(1556521739139L)(Day17.answer2)

  test("Day18"):
    assertResult(4340)(Day18.answer1)
    assertResult(2468)(Day18.answer2)

  test("Day19"):
    assertResult(1177)(Day19.answer1)
    assertResult(62744)(Day19.answer2)

  test("Day20"):
    assertResult(8302L)(Day20.answer1)
    assertResult(656575624777L)(Day20.answer2)

  test("Day21"):
    assertResult(282285213953670L)(Day21.answer1)
    assertResult(3699945358564L)(Day21.answer2)

  test("Day22"):
    assertResult(56372)(Day22.answer1)
    assertResult(197047)(Day22.answer2)

  test("Day23"):
    assertResult(4109)(Day23.answer1)
    assertResult(1055)(Day23.answer2)

  test("Day24"):
    assertResult(292)(Day24.answer1)
    assertResult(816)(Day24.answer2)

  test("Day25"):
    assertResult("2=112--220-=-00=-=20")(Day25.answer1)

  test("Day26"):
    assertResult(20)(Day26.answer10)
