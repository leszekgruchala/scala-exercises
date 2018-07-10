package eu.gruchala.trampolines

import eu.gruchala.BaseSpec

class TrampolinedStateSpecs extends BaseSpec {

  "Trampolined State implementation" should {
    "safely finish execution" when {
      "combined with zipIndex function" in {
        //Seem to be working random on Scala 2.12
        TStateOps.zipIndex(List.fill(10000)("A")) should not be empty
      }
    }
  }
}
