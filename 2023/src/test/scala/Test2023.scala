
import nmcb.*

import aoc2023.*
import org.scalatest.funsuite.AnyFunSuite

class Test2023 extends AnyFunSuite:
  test("Day 1: Trebuchet?!"):
    assertResult(54916)(Day01.answer1)
    assertResult(54728)(Day01.answer2)

  test("Day 2: Cube Conundrum"):
    assertResult(2600)(Day02.answer1)
    assertResult(86036)(Day02.answer2)

  test("Day 3: Gear Ratios"):
    assertResult(498559)(Day03.answer1)
    assertResult(72246648)(Day03.answer2)

  test("Day 4: Scratchcards"):
    assertResult(24175)(Day04.answer1)
    assertResult(18846301)(Day04.answer2)

  test("Day 5: If You Give A Seed A Fertilizer"):
    assertResult(51580674)(Day05.answer1)
    assertResult(99751240)(Day05.answer2)

  test("Day 6: Wait For It"):
    assertResult(1413720)(Day06.answer1)
    assertResult(30565288)(Day06.answer2)

  test("Day 7: Camel Cards"):
    assertResult(250347426)(Day07.answer1)
    assertResult(251224870)(Day07.answer2)

  test("Day 8: Haunted Wasteland"):
    assertResult(18673)(Day08.answer1)
    assertResult(17972669116327L)(Day08.answer2)

  test("Day 9: Mirage Maintenance"):
    assertResult(1798691765)(Day09.answer1)
    assertResult(1104)(Day09.answer2)

  test("Day 10: Pipe Maze"):
    assertResult(6733)(Day10.answer1)
    assertResult(435)(Day10.answer2)

  test("Day 11: Cosmic Expansion"):
    assertResult(9947476)(Day11.answer1)
    assertResult(519939907614L)(Day11.answer2)

  test("Day 12: Hot Springs"):
    assertResult(7506)(Day12.answer1)
    assertResult(548241300348335L)(Day12.answer2)

  test("Day 13: Point of Incidence"):
    assertResult(33780)(Day13.answer1)
    assertResult(23479)(Day13.answer2)

  test("Day 14: Parabolic Reflector Dish"):
    assertResult(110677)(Day14.answer1)
    assertResult(90551)(Day14.answer2)

  test("Day 15: Lens Library"):
    assertResult(506869)(Day15.answer1)
    assertResult(271384)(Day15.answer2)

  test("Day 16: The Floor Will Be Lava"):
    assertResult(7415)(Day16.answer1)
    assertResult(7943)(Day16.answer2)

  test("Day 17: Clumsy Crucible"):
    assertResult(963)(Day17.answer1)
    assertResult(1178)(Day17.answer2)

  test("Day 18: Lavaduct Lagoon"):
    assertResult(52055)(Day18.answer1)
    assertResult(67622758357096L)(Day18.answer2)

  test("Day 19: Aplenty") {
    assertResult(449531)(Day19.answer1)
    assertResult(122756210763577L)(Day19.answer2)
  }
  test("Day 20: Pulse Propagation") {
    assertResult(812609846)(Day20.answer1)
    assertResult(245114020323037L)(Day20.answer2)
  }
  test("Day 21: Step Counter") {
    assertResult(3788)(Day21.answer1)
    assertResult(631357596621921L)(Day21.answer2)
  }
  test("Day 22: Sand Slabs") {
    assertResult(454)(Day22.answer1)
    assertResult(74287)(Day22.answer2)
  }
  test("Day 23: A Long Walk") {
    assertResult(2430)(Day23.answer1)
    assertResult(6534)(Day23.answer2)
  }
  test("Day 24: Never Tell Me The Odds") {
    assertResult(14799)(Day24.answer1)
    assertResult(1007148211789625L)(Day24.answer2)
  }
  test("Day 25: Snowverload") {
    assertResult(569904)(Day25.answer1)
  }
