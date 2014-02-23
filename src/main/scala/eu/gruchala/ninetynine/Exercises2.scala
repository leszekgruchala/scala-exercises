package eu.gruchala.ninetynine

import scala.annotation.tailrec

object Exercises2 {

  def removeDuplicates(list: List[Int]) = {
    @tailrec
    def iter(current: Int, restOfList: List[Int], acc: List[Int]): List[Int] = {
      if (!restOfList.isEmpty) {
        if (acc.head != current) {
          iter(restOfList.head, restOfList.tail, current :: acc)
        } else {
          iter(restOfList.head, restOfList.tail, acc)
        }
      } else {
        val r = current :: acc
        r.reverse
      }
    }

    iter(list.head, list.tail, list.head :: Nil)
  }

  def nth(n: Int, l: List[Int]): Int = {
    @tailrec
    def iter(iteration: Int): Int = {
      if (iteration == n) {
        l(iteration)
      } else {
        iter(iteration + 1)
      }
    }
    iter(0)
  }

  def lastButOne(l: List[Int]): Int = {
    l.reverse.tail.head
  }

  def sumOfMultiples(lower: Int, upper: Int) = {
    val range = (lower to upper).toList
    range.filter(x => x % 3 == 0 || x % 5 == 0).sum
  }

  def toAsci(chars: List[Char]): List[Int] = {
    chars map (_ toInt)
  }
}
