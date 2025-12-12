package aoc2025

import org.scalatest.funsuite.AnyFunSuite

class Test2025 extends AnyFunSuite:
  test("Day 1: Secret Entrance"):
    assertResult(1055)(Day01.answer1)
    assertResult(6386)(Day01.answer2)

  test("Day 2: Gift Shop"):
    assertResult(23701357374L)(Day02.answer1)
    assertResult(34284458938L)(Day02.answer2)

  test("Day 3: Lobby"):
    assertResult(17074L)(Day03.answer1)
    assertResult(169512729575727L)(Day03.answer2)

  test("Day 4: Printing Department"):
    assertResult(1493)(Day04.answer1)
    assertResult(9194)(Day04.answer2)

  test("Day 5: Cafeteria"):
    assertResult(601L)(Day05.answer1)
    assertResult(367899984917516L)(Day05.answer2)

  test("Day 6: Trash Compactor"):
    assertResult(5552221122013L)(Day06.answer1)
    assertResult(11371597126232L)(Day06.answer2)

  test("Day 7: Laboratories"):
    assertResult(1660L)(Day07.answer1)
    assertResult(305999729392659L)(Day07.answer2)

  test("Day 8: Playground"):
    assertResult(171503L)(Day08.answer1)
    assertResult(9069509600L)(Day08.answer2)

  test("Day 9: Movie Theater"):
    assertResult(4749672288L)(Day09.answer1)
    assertResult(1479665889L)(Day09.answer2)

  test("Day 10: Factory"):
    assertResult(542)(Day10.answer1)
    assertResult(20871)(Day10.answer2)

  test("Day 11: Reactor"):
    assertResult(590)(Day11.answer1)
    assertResult(319473830844560L)(Day11.answer2)

  test("Day 12: Christmas Tree Farm"):
    assertResult(590)(Day11.answer1)
    assertResult(319473830844560L)(Day11.answer2)