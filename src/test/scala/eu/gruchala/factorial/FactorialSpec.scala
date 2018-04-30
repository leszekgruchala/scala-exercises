package eu.gruchala.factorial

import eu.gruchala.BaseSpec
import eu.gruchala.factorial.Factorial._

class FactorialSpec extends BaseSpec {

  "Factorial" should {
    "factorial" in {
      factorial(2) shouldEqual 2
      factorial(4) shouldEqual 24
      factorial(20) shouldEqual 2432902008176640000L
    }

    "factorial2" in {
      factorial2(2) shouldEqual 2
      factorial2(4) shouldEqual 24
      factorial2(20) shouldEqual 2432902008176640000L
    }
  }
}
