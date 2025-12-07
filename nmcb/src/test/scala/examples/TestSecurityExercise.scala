package examples

import examples.SecurityExercise
import org.scalatest.funsuite.AnyFunSuite

class TestSecurityExercise extends AnyFunSuite:
  test("SecurityExercise") {
    assertResult(766)(SecurityExercise.cluster0)
    assertResult(744)(SecurityExercise.cluster1)
  }
