import org.scalatest.funsuite.AnyFunSuite

class TestSecurityExercise extends AnyFunSuite:
  test("SecurityExercise") {
    assertResult(766)(actual = SecurityExercise.cluster0)
    assertResult(744)(actual = SecurityExercise.cluster1)
  }
