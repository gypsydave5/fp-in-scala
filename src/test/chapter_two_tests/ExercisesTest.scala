package chapter_two_tests

import org.scalatest.FunSuite
import chapter_two.Exercises._
import chapter_two.HelloImAFunction

class ExercisesTest extends FunSuite {
  test("isSorted") {
    val a = Array(1, 2,3, 4, 5, 6)
    assert(isSorted[Int](a, _ < _))
    assertResult(isSorted[Int](a, _ > _))(false)
  }

  test("curry") {
    assert(HelloImAFunction(2, 7) == curry(HelloImAFunction)(2)(7))
  }

  test("uncurry") {
    assert(uncurry(curry(HelloImAFunction))(2, 5) == 7)
  }

  test("compose") {
    assert(compose((x: Int) => x * 3, (x: Int) => x + 2)(1) == 9)
  }
}
