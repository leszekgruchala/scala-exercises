package eu.gruchala.primes

object Primes {

  /**
    * A natural number greater than 1 that can only be divided by 1 and itself.
    */
  def isPrime(n: Int): Boolean =
    n > 1 && (2 until n).forall(n % _ != 0)

  def primesUpTo(n: Int): Seq[Int] =
    (2 until n).filter(isPrime)
}
