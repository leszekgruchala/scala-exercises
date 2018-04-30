package eu.gruchala

import scala.collection.mutable

//generalnie na płaszczyźnie dwuwymiarowej odległość między dwoma punktami
//to pierwiastek z sumy kwadratów x, y

case class Point(x: Int, y: Int, z: Int) {
  lazy val dist: Double = math.sqrt(List(x, y, z).map(_.toDouble).map(math.pow(_, 2)).sum)
}
object Points {

  def closest(input: Iterator[Point], k: Int): List[Point] = {
    val mem: mutable.Set[Point] = mutable.Set.empty

    while (input.hasNext) {
      val current = input.next()

      val largerElems = mem.filter(_.dist > current.dist)
      val hasLargerElems = largerElems.nonEmpty

      val shouldAppend = k > mem.size
      val hasNoSpace = mem.size >= k

      if (hasLargerElems && hasNoSpace) {
        val maxPoint = largerElems.max(Ordering.by[Point, Double](_.dist))
        mem -= maxPoint
      }
      if (shouldAppend || hasLargerElems) mem += current
    }

    mem.toList
  }
}
