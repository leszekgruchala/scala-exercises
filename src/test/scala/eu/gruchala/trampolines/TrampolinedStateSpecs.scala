package eu.gruchala.trampolines

import eu.gruchala.BaseSpec

class TrampolinedStateSpecs extends BaseSpec {

  //Seem to be working randomly on Scala 2.12
  "Trampolined State implementation" ignore {
    "safely finish execution" when {
      "combined with zipIndex function" in {
        TStateOps.zipIndex(List.fill(10000)("A")) should not be empty
      }
    }
  }
}
