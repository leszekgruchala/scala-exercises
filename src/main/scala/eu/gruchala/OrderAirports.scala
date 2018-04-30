package eu.gruchala

import scala.annotation.tailrec

object OrderAirports {


  //  Having a list of airline tickets List("KRK" -> "GDN", "KTA" -> "WRO", "WRO" -> "KRK")
  // return a list specifying airport visiting order List("KTA", "WRO", "KRK", "GDN")

  def orderAirports(l: List[(String, String)]): Set[String] = {
    @tailrec
    def loop(rest: List[(String, String)], acc: Set[String]): Set[String] = rest match {
      case Nil => acc
      case (from, to) :: t =>
        val restFrom = t.map(_._1)
        val restTo = t.map(_._2)
        if (!restTo.contains(from)) {
          loop(t, acc ++ List(from, to))
        } else if (restFrom.contains(from)) {
          loop(t, acc ++ List(from, to))
        } else {
          loop(t ::: List((from, to)), acc)
        }
    }

    loop(l, Set.empty)
  }

  def orderAirports2(xs: List[(String, String)]): List[String] = {
    val srcToDst = xs.toMap

    val first = srcToDst.keySet.filterNot(v => srcToDst.values.exists(_ == v)).head

    def recurse(src: String, route: List[String]): List[String] = {
      srcToDst.get(src) match {
        case None => src :: route
        case Some(dst) => recurse(dst, src :: route)
      }
    }

    recurse(first, List.empty).reverse
  }

  println(orderAirports(List("KRK" -> "GDN", "KTA" -> "WRO", "WRO" -> "KRK")))
  println(orderAirports2(List("KRK" -> "GDN", "KTA" -> "WRO", "WRO" -> "KRK")))
}
