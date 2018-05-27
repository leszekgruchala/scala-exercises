package eu.gruchala.factorial

import scala.annotation.tailrec

object Factorial {

  def factorial(n: Int): Long = {
    @tailrec
    def loop(rest: Int, acc: Long): Long = {
      if (rest == 0) acc
      else loop(rest - 1, acc * rest)
    }

    loop(n, 1)
  }

  def factorial2(n: Int): Long = {
    @tailrec
    def loop(rest: Int, acc: Long): Long = rest match {
      case 0 => acc
      case r => loop(r - 1, r * acc)
    }

    loop(n, 1)
  }
}
