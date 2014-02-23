package eu.gruchala.ninetynine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class Exercises2Suite extends FunSuite {

  test("should get nth element") {
    val nth: Int = Exercises2.nth(3, List(5, 2, 1, 6, 8, 3))

    assert(nth === 6)
  }

  test("should get next to last element") {
    val nextToLast: Int = Exercises2.lastButOne(List(5, 2, 1, 6, 8, 3))

    assert(nextToLast === 8)
  }

  test("remove next duplicates") {
    val list = Exercises2.removeDuplicates(List(1, 1, 1, 2, 2, 1, 1, 3))

    assert(list === List(1, 2, 1, 3))
  }

  test("sumOfMultiples") {
    val r = Exercises2.sumOfMultiples(1, 10)

    assert(r === 33)
  }

  test("ascii") {
    val r = Exercises2.toAsci(List('a', 'b', 'c'))
    assert(r === List(97, 98, 99))
  }
}
