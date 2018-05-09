package eu.gruchala.fibonacci

import scala.annotation.tailrec

//Each 2 proceeding numbers are the sum of the next one
//1 1 2 3 5 8 13 21..
object Fibonacci {

  def fib(iterations: Int): Int = {
    @tailrec
    def loop(left: Int, a: Int, b: Int): Int = left match {
      case 0 => a
      case 1 => b
      case l => loop(l - 1, b, a + b)
    }

    loop(iterations, 0, 1)
  }

  def fibStream(iterations: Int): Int = {

    def loop(a: Int, b: Int): Stream[Int] = a #:: loop(b, a + b)

    loop(1, 1).take(iterations).reverse.headOption.getOrElse(0)
  }
}
