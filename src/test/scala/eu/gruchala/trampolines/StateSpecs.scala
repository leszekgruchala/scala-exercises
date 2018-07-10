package eu.gruchala.trampolines

import scala.compat.Platform.StackOverflowError

import eu.gruchala.BaseSpec

class StateSpecs extends BaseSpec {

  "Default State implementation" should {
    "throw a StackOverflowException" when {
      "combined with zipIndex function" in {
        a[StackOverflowError] shouldBe thrownBy {
          StateOps.zipIndex(List.fill(10000)("A"))
        }
      }
    }
  }
}
