package eu.gruchala.trees

import scala.annotation.tailrec

object BinarySearch {

  //Binary search of sorted array
  def binarySearch(arr: Vector[Int], x: Int): Boolean = {
    @tailrec
    def loop(rest: Vector[Int], n: Int): Boolean = {
      rest match {
        case Vector() => false
        case Vector(elem) => elem == n
        case _ =>
          val index = rest.size / 2
          val elem = rest(index)
          if (elem <= n) loop(rest.splitAt(index)._2, n)
          else loop(rest.splitAt(index)._1, n)
      }
    }

    loop(arr, x)
  }
}
