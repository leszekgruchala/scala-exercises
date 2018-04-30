package eu.gruchala.factorial

object Factorial {

  def factorial(n: Int): Long = {
    def loop(rest: Int, acc: Long): Long = {
      if (rest == 0) acc
      else loop(rest - 1, acc * rest)
    }

    loop(n, 1)
  }

  def factorial2(n: Int): Long = {

    def loop(rest: Int, acc: Long): Long = rest match {
      case 0 => acc
      case r => loop(r - 1, r * acc)
    }

    loop(n, 1)
  }
}
