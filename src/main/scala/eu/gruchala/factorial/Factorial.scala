package eu.gruchala.factorial

object Factorial {
  def factorial(n: Int): Int = {

    def loop(rest: Int, acc: Int): Int = {
      if (rest == 0) acc
      else loop(rest - 1, acc * rest)
    }

    loop(n, 1)
  }
}
