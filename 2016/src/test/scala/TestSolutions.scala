import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(288)(Day01.answer1)
    assertResult(111)(Day01.answer2)

  test("Day02"):
    assertResult("38961")(Day02.answer1)
    assertResult("46C92")(Day02.answer2)

  test("Day03"):
    assertResult( 869)(Day03.answer1)
    assertResult(1544)(Day03.answer2)

  test("Day04"):
    assertResult(185371)(Day04.answer1)
    assertResult(   984)(Day04.answer2)

  test("Day05"):
    assertResult("c6697b55")(Day05.answer1)
    assertResult("8c35d1ab")(Day05.answer2)

  test("Day06"):
    assertResult("afwlyyyq")(Day06.answer1)
    assertResult("bhkzekao")(Day06.answer2)

  test("Day07"):
    assertResult(118)(Day07.answer1)
    assertResult(260)(Day07.answer2)

  test("Day08"):
    assertResult(110)(Day08.answer1)
    assertResult("""####...##.#..#.###..#..#..##..###..#....#...#..##.
                   |...#....#.#..#.#..#.#.#..#..#.#..#.#....#...#...#.
                   |..#.....#.####.#..#.##...#....#..#.#.....#.#....#.
                   |.#......#.#..#.###..#.#..#....###..#......#.....#.
                   |#....#..#.#..#.#.#..#.#..#..#.#....#......#..#..#.
                   |####..##..#..#.#..#.#..#..##..#....####...#...##.."""
                   .stripMargin)(Day08.answer2)

  test("Day09"):
    assertResult(112830)(Day09.answer1)
    assertResult(10931789799L)(Day09.answer2)

  test("Day10"):
    assertResult(98)(Day10.answer1)
    assertResult(4042)(Day10.answer2)

  test("Day11"):
    assertResult(47)(Day11.answer1)
    assertResult(71)(Day11.answer2)

  test("Day12"):
    assertResult(318007)(Day12.answer1)
    assertResult(9227661)(Day12.answer2)

  test("Day13"):
    assertResult(90)(Day13.answer1)
    assertResult(135)(Day13.answer2)

  test("Day14"):
    assertResult(16106)(Day14.answer1)
    assertResult(22423)(Day14.answer2)

  test("Day15"):
    assertResult(203660)(Day15.answer1)
    assertResult(2408135)(Day15.answer2)

  test("Day16"):
    assertResult("10101001010100001")(Day16.answer1)
    assertResult("10100001110101001")(Day16.answer2)

  test("Day17"):
    assertResult("DRDRULRDRD")(Day17.answer1)
    assertResult(384)(Day17.answer2)

  test("Day18"):
    assertResult(1978)(Day18.answer1)
    assertResult(20003246)(Day18.answer2)

  test("Day19"):
    assertResult(1841611)(Day19.answer1)
    assertResult(1423634)(Day19.answer2)

  test("Day20"):
    assertResult(31053880)(Day20.answer1)
    assertResult(117)(Day20.answer2)

  test("Day21"):
    assertResult("dgfaehcb")(Day21.answer1)
    assertResult("fdhgacbe")(Day21.answer2)

  test("Day22"):
    assertResult(934)(Day22.answer1)
    assertResult(207)(Day22.answer2)

  test("Day23"):
    assertResult(10152)(Day23.answer1)
    assertResult(479006712)(Day23.answer2)

  test("Day24"):
    assertResult(456)(Day24.answer1)
    assertResult(704)(Day24.answer2)

  test("Day25"):
    assertResult(196)(Day25.answer1)
    assertResult(50)(Day25.answer2)
