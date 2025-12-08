import aoc2021.{Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25}
import org.scalatest.funsuite.AnyFunSuite

class Test2021 extends AnyFunSuite:

  test("Day 1: Sonar Sweep"):
    assertResult(1602)(Day01.answer1)
    assertResult(1633)(Day01.answer2)

  test("Day 2: Dive!"):
    assertResult(2322630)(Day02.answer1)
    assertResult(2105273490)(Day02.answer2)

  test("Day 3: Binary Diagnostic"):
    assertResult(2648450)(Day03.answer1)
    assertResult(2845944)(Day03.answer2)

  test("Day 4: Giant Squid"):
    assertResult(21607)(Day04.answer1)
    assertResult(19012)(Day04.answer2)

  test("Day 5: Hydrothermal Venture"):
    assertResult(7085)(Day05.answer1)
    assertResult(20271)(Day05.answer2)

  test("Day 6: Lanternfish"):
    assertResult(380612)(Day06.answer1)
    assertResult(1710166656900L)(Day06.answer2)

  test("Day 7: The Treachery of Whales"):
    assertResult(328187)(Day07.answer1)
    assertResult(91257582)(Day07.answer2)

  test("Day 8: Seven Segment Search"):
    assertResult(521)(Day08.answer1)
    assertResult(1016804)(Day08.answer2)

  test("Day 9: Smoke Basin"):
    assertResult(502)(Day09.answer1)
    assertResult(1330560)(Day09.answer2)

  test("Day 10: Syntax Scoring"):
    assertResult(268845)(Day10.answer1)
    assertResult(4038824534L)(Day10.answer2)

  test("Day 11: Dumbo Octopus"):
    assertResult(1732)(Day11.answer1)
    assertResult(290)(Day11.answer2)

  test("Day 12: Passage Pathing"):
    assertResult(5252)(Day12.answer1)
    assertResult(147784)(Day12.answer2)

  test("Day 13: Transparent Origami"):
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

  test("Day 14: Extended Polymerization"):
    assertResult(2194)(Day14.answer1)
    assertResult(2360298895777L)(Day14.answer2)

  test("Day 15: Chiton"):
    assertResult(652)(Day15.answer1)
    assertResult(2938)(Day15.answer2)

  test("Day 16: Packet Decoder"):
    assertResult(886)(Day16.answer1)
    assertResult(184487454837L)(Day16.answer2)

  test("Day 17: Trick Shot"):
    assertResult(9180)(Day17.answer1)
    assertResult(3767)(Day17.answer2)

  test("Day 18: Snailfish"):
    assertResult(3935)(Day18.answer1)
    assertResult(4669)(Day18.answer2)

  test("Day 19: Beacon Scanner"):
    assertResult(320)(Day19.answer1)
    assertResult(9655)(Day19.answer2)

  test("Day 20: Trench Map"):
    assertResult(5298)(Day20.answer1)
    assertResult(17548)(Day20.answer2)

  test("Day 21: Dirac Dice"):
    assertResult(679329)(Day21.answer1)
    assertResult(433315766324816L)(Day21.answer2)

  test("Day 22: Reactor Reboot"):
    assertResult(542711)(Day22.answer1)
    assertResult(1160303042684776L)(Day22.answer2)

  test("Day 23: Amphipod"):
    assertResult(11417)(Day23.answer1)
    assertResult(49529)(Day23.answer2)

  test("Day 24: Arithmetic Logic Unit"):
    assertResult("39999698799429")(Day24.answer1)
    assertResult("18116121134117")(Day24.answer2)

  test("Day 25: Sea Cucumber"):
    assertResult(582)(Day25.answer1)
