package eu.gruchala.ninetynine

import scala.annotation.tailrec

object Exercises3 {

  //p19
  def rotate(i: Int, l: List[Symbol]) = {
    val at = if (i < 0) l.splitAt(l.size + i) else l.splitAt(i)
    at._2 ::: at._1
  }

  //p16
  def drop(interval: Int, l: List[Symbol]): List[Symbol] = {
    @tailrec
    def iter(list: List[Symbol], acc: List[Symbol]): List[Symbol] = list match {
      case Nil => acc.reverse
      case head :: tail =>
        val index = l.indexOf(head) + 1
        if (index % interval != 0) {
          iter(tail, head :: acc)
        } else {
          iter(tail, acc)
        }
    }
    iter(l, Nil)
  }

  //p12
  def decode(tuples: List[(Int, Symbol)]) = {
    @tailrec
    def iter(curList: List[(Int, Symbol)], acc: List[Symbol]): List[Symbol] = curList match {
      case head :: tail => iter(tail, acc ::: List.fill(head._1)(head._2))
      case Nil => acc
    }

    iter(tuples, Nil)
  }
}
