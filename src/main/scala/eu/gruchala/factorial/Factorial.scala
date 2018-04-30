package eu.gruchala.factorial

object Factorial {
  def factorial(n: Int): Long = {
    def loop(rest: Int, acc: Long): Long = {
      if (rest == 0) acc
      else loop(rest - 1, acc * rest)
    }

    loop(n, 1)
  }
}
