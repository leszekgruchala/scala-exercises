package eu.gruchala.ninetynine

import scala.annotation.tailrec

object Exercises2 {

  def removeDuplicates(list: List[Int]): List[Int] = {
    @tailrec
    def loop(current: Int, restOfList: List[Int], acc: List[Int]): List[Int] = {
      if (restOfList.nonEmpty) {
        if (acc.head != current) {
          loop(restOfList.head, restOfList.tail, current :: acc)
        } else {
          loop(restOfList.head, restOfList.tail, acc)
        }
      } else {
        val r = current :: acc
        r.reverse
      }
    }

    loop(list.head, list.tail, list.head :: Nil)
  }

  def nth(n: Int, l: List[Int]): Int = {
    @tailrec
    def loop(iteration: Int): Int = {
      if (iteration == n) {
        l(iteration)
      } else {
        loop(iteration + 1)
      }
    }
    loop(0)
  }

  def lastButOne(l: List[Int]): Int = {
    l.reverse.tail.head
  }

  def sumOfMultiples(lower: Int, upper: Int): Int = {
    val range = (lower to upper).toList
    range.filter(x => x % 3 == 0 || x % 5 == 0).sum
  }

  def toAsci(chars: List[Char]): List[Int] = {
    chars.map(_.toInt)
  }
}
