package eu.gruchala.primes

import eu.gruchala.BaseSpec
import eu.gruchala.primes.Primes._

class PrimesSpec extends BaseSpec {

  "isPrime must return correct result" in {
    isPrime(0) shouldEqual false
    isPrime(1) shouldEqual false
    isPrime(2) shouldEqual true
    isPrime(3) shouldEqual true
    isPrime(4) shouldEqual false
    isPrime(5) shouldEqual true
    isPrime(6) shouldEqual false
    isPrime(7) shouldEqual true
    isPrime(10) shouldEqual false
    isPrime(19) shouldEqual true
  }

  "primesUpTo should return only prime numbers" in {
    primesUpTo(100) shouldEqual Seq(
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
    )
  }
}
