package eu.gruchala.arrays

import eu.gruchala.BaseSpec

class TwoArraysSortSpec extends BaseSpec {

  val array1 = Array(3, 6, 7, 9, 200, 204, 207, 2009)
  val array2 = Array(1, 2, 11, 12, 200, 201)

  "combined with zipIndex function" in {
    val result = TwoArraysSort.max(2, array1, array2)
    result shouldEqual 207
  }
}
