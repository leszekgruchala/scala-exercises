package eu.gruchala.fibonacci

import scala.annotation.tailrec

//Each 2 proceeding numbers are the sum of the next one
//1 1 2 3 5 8 13 21..
object Fibonacci {

  def fib(iteration: Int): Int = {
    @tailrec
    def loop(left: Int, a: Int, b: Int): Int = left match {
      case 0 => a
      case 1 => b
      case l => loop(l - 1, b, a + b)
    }

    loop(iteration, 0, 1)
  }
}
