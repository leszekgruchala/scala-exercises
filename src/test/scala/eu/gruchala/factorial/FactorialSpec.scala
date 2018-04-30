package eu.gruchala.factorial

import eu.gruchala.BaseSpec
import eu.gruchala.factorial.Factorial._

class FactorialSpec extends BaseSpec {

  "Factorial" should {
    "do the job" in {
      factorial(2) shouldEqual 4
      factorial(4) shouldEqual 16
      factorial(20) shouldEqual 2432902008176640000L
    }
  }
}
