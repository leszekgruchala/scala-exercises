package eu.gruchala.trees

import eu.gruchala.BaseSpec

class BinarySearchSpec extends BaseSpec {

  val sample = IndexedSeq.range(1, 10).take(10).toVector

  "Binary Search" should {
    "return true if number is found" in {
      BinarySearch.binarySearch(sample, 4) shouldBe true
    }
    "return false if number is found" in {
      BinarySearch.binarySearch(sample, 123) shouldBe false
    }
  }
}
