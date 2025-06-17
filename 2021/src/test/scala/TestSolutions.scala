import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(1602)(Day01.answer1)
    assertResult(1633)(Day01.answer2)

  test("Day02"):
    assertResult(2322630)(Day02.answer1)
    assertResult(2105273490)(Day02.answer2)

  test("Day03"):
    assertResult(2648450)(Day03.answer1)
    assertResult(2845944)(Day03.answer2)

  test("Day04"):
    assertResult(21607)(Day04.answer1)
    assertResult(19012)(Day04.answer2)

  test("Day05"):
    assertResult(7085)(Day05.answer1)
    assertResult(20271)(Day05.answer2)

  test("Day06"):
    assertResult(380612)(Day06.answer1)
    assertResult(1710166656900L)(Day06.answer2)

  test("Day07"):
    assertResult(328187)(Day07.answer1)
    assertResult(91257582)(Day07.answer2)

  test("Day08"):
    assertResult(521)(Day08.answer1)
    assertResult(1016804)(Day08.answer2)

  test("Day09"):
    assertResult(502)(Day09.answer1)
    assertResult(1330560)(Day09.answer2)

  test("Day10"):
    assertResult(268845)(Day10.answer1)
    assertResult(4038824534L)(Day10.answer2)

  test("Day11"):
    assertResult(1732)(Day11.answer1)
    assertResult(290)(Day11.answer2)

  test("Day12"):
    assertResult(5252)(Day12.answer1)
    assertResult(147784)(Day12.answer2)

  test("Day13"):
    assertResult(763)(Day13.answer1)
    assertResult(
      """
        |###..#..#..##..#....###...##..###...##.
        |#..#.#..#.#..#.#....#..#.#..#.#..#.#..#
        |#..#.####.#..#.#....#..#.#....#..#.#..#
        |###..#..#.####.#....###..#....###..####
        |#.#..#..#.#..#.#....#.#..#..#.#.#..#..#
        |#..#.#..#.#..#.####.#..#..##..#..#.#..#
        |""".stripMargin)(Day13.answer2)

  test("Day14"):
    assertResult(2194)(Day14.answer1)
    assertResult(2360298895777L)(Day14.answer2)

  test("Day15"):
    assertResult(652)(Day15.answer1)
    assertResult(2938)(Day15.answer2)

  test("Day16"):
    assertResult(886)(Day16.answer1)
    assertResult(184487454837L)(Day16.answer2)

  test("Day17"):
    assertResult(9180)(Day17.answer1)
    assertResult(3767)(Day17.answer2)

  test("Day18"):
    assertResult(3935)(Day18.answer1)
    assertResult(4669)(Day18.answer2)

  test("Day19"):
    assertResult(320)(Day19.answer1)
    assertResult(9655)(Day19.answer2)

  test("Day20"):
    assertResult(5298)(Day20.answer1)
    assertResult(17548)(Day20.answer2)

  test("Day21"):
    assertResult(679329)(Day21.answer1)
    assertResult(433315766324816L)(Day21.answer2)

  test("Day22"):
    assertResult(542711)(Day22.answer1)
    assertResult(1160303042684776L)(Day22.answer2)

  test("Day23"):
    assertResult(11417)(Day23.answer1)
    assertResult(49529)(Day23.answer2)

  test("Day24"):
    assertResult("39999698799429")(Day24.answer1)
    assertResult("18116121134117")(Day24.answer2)

  test("Day25"):
    assertResult(582)(Day25.answer1)
