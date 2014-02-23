package eu.gruchala.ninetynine

object Exercises {

  def p01(list: List[Int]) = {
    list.last
  }

  def p01_1(l: List[Int]) = {
    l.reverse.head
  }

  def p01_2(l: List[Int]) = {
    l(l.size - 1)
  }

  def p02(l: List[Int]) = {
    l.reverse.tail.head
  }

  def p02_02(l: List[Int]) = {
    l.takeRight(2).head
  }

  def p03(k: Int, l: List[Int]) = {
    l(k)
  }

  def p04(l: List[Int]) = {
    l.size
  }

  def p04_02(l: List[Int]) = {
    var counter = 0
    for (i <- l) {
      counter += 1
    }
    counter
  }

  def p04_03(l: List[Int]) = {
    l.foldRight(0) {
      (cur, acc) => acc + 1
    }
  }

  def p12(tuples: List[(Int, Symbol)]) = {
    val r = for (tuple <- tuples) yield {
      List.fill(tuple._1)(tuple._2) :: Nil
    }
    r.flatten.flatten
  }
}
