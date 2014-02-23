package eu.gruchala.ninetynine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class ExercisesSuite extends FunSuite {

  test("find last element of a list") {
    val last = Exercises.p01(List(2, 4, 1, 6, 7, 3, 1))
    val last_01 = Exercises.p01_1(List(2, 4, 1, 6, 7, 3, 1))
    val last_02 = Exercises.p01_2(List(2, 4, 1, 6, 7, 3, 1))
    assert(last === 1)
    assert(last_01 === 1)
    assert(last_02 === 1)
  }

  test("Find the last but one element of a list") {
    val r = Exercises.p02(List(1, 1, 2, 3, 5, 8))
    val r_02 = Exercises.p02_02(List(1, 1, 2, 3, 5, 8))
    assert(r === 5)
    assert(r_02 === 5)
  }

  test("Find the Kth element of a list") {
    val r = Exercises.p03(2, List(1, 1, 2, 3, 5, 8))
    assert(r === 2)
  }

  test("Find the number of elements of a list") {
    val r = Exercises.p04(List(1, 1, 2, 3, 5, 8))
    val r_02 = Exercises.p04_02(List(1, 1, 2, 3, 5, 8))
    val r_03 = Exercises.p04_02(List(1, 1, 2, 3, 5, 8))
    assert(r === 6)
    assert(r_02 === 6)
    assert(r_03 === 6)
  }

  test("Decode a run-length encoded list") {
    val r = Exercises.p12(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(r === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
}
