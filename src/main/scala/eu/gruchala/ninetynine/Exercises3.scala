package eu.gruchala.ninetynine

import scala.annotation.tailrec

object Exercises3 {

  //p19
  def rotate(i: Int, l: List[Symbol]): List[Symbol] = {
    val at = if (i < 0) l.splitAt(l.size + i) else l.splitAt(i)
    at._2 ::: at._1
  }

  //p16
  def drop(interval: Int, l: List[Symbol]): List[Symbol] = {
    @tailrec
    def loop(list: List[Symbol], acc: List[Symbol]): List[Symbol] = list match {
      case Nil => acc.reverse
      case head :: tail =>
        val index = l.indexOf(head) + 1
        if (index % interval != 0) {
          loop(tail, head :: acc)
        } else {
          loop(tail, acc)
        }
    }
    loop(l, Nil)
  }

  //p12
  def decode(tuples: List[(Int, Symbol)]): List[Symbol] = {
    @tailrec
    def loop(curList: List[(Int, Symbol)], acc: List[Symbol]): List[Symbol] = curList match {
      case head :: tail => loop(tail, acc ::: List.fill(head._1)(head._2))
      case Nil => acc
    }

    loop(tuples, Nil)
  }
}
