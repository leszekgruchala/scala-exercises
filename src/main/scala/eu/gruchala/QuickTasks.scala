package eu.gruchala

import scala.annotation.tailrec
import scala.util.Try

object QuickTasks extends App {

  //  Implement findMax(l: List[Int]): Option[Int] using tail recursion
  def findMax(l: List[Int]): Option[Int] = {
    @tailrec
    def loop(rest: List[Int], max: Int): Int = rest match {
      case Nil => max
      case h :: t =>
        if (h > max) loop(t, h) else loop(t, max)
    }

    if (l.isEmpty) None else Some(loop(l, 0))
  }

  //  Implement findMax(l: List[Int]): Option[Int] using foldLeft
  //for empty list can return Some(0)
  def findMaxFold(l: List[Int]): Option[Int] =
  l.foldLeft(Option.empty[Int])((acc, elem) => acc.map(res => if (elem > res) elem else res))

  def findMaxReduce(l: List[Int]): Option[Int] =
    l.reduceLeftOption((acc, elem) => if (elem > acc) elem else acc)

  //  Implement "123".convertToInt which returns an Option[Int] instead of possibly throwing exceptions
  implicit class ConvertToInt(s: String) {

    def convertToInt: Option[Int] = {
      Try(s.toInt).toOption
    }
  }
}
