package eu.gruchala.fibonacci

import scala.annotation.tailrec

class Fibonacci {

  def fib(n: Int): Int = {

    @tailrec
    def loop(left: Int, a: Int, b: Int): Int = left match {
      case 0 => a
      case 1 => b
      case _ => loop(left - 1, b, a + b)
    }

    loop(n, 0, 1)
  }
}
