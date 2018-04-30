package eu.gruchala

import eu.gruchala.SomeUp._
import org.scalatest.{Matchers, WordSpec}

class SomeUpSpecs extends WordSpec with Matchers {

  "Summing two digits" should {
    "return true" when {
      "1" in {
        doesItSumUpTo(Seq(1, 2, 3), 3) shouldBe true
      }

      "2" in {
        doesItSumUpTo(Seq(1, 4, 5, 3), 9) shouldBe true
      }
    }

    "return false" when {
      "1" in {
        doesItSumUpTo(Seq(1, 4, 5, 3), 19) shouldBe false
      }

      "2" in {
        doesItSumUpTo(Seq(1, 4, 5, 3), 2) shouldBe false
      }
    }
  }

}
