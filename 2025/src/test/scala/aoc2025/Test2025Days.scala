package aoc2025

import org.scalatest.funsuite.AnyFunSuite

class Test2025Days extends AnyFunSuite:
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