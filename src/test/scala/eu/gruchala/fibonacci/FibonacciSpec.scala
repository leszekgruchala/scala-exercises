package eu.gruchala.fibonacci

import eu.gruchala.BaseSpec
import eu.gruchala.fibonacci.Fibonacci._

class FibonacciSpec extends BaseSpec {

  "fib" in {
    fib(0) shouldEqual 0
    fib(1) shouldEqual 1
    fib(2) shouldEqual 1
    fib(3) shouldEqual 2
    fib(4) shouldEqual 3
    fib(6) shouldEqual 8
    fib(10) shouldEqual 55
  }
}
