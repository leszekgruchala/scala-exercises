package eu.gruchala

object SomeUp {

  def doesItSumUpTo(numbers: Seq[Int], k: Int): Boolean = {
    val histo = numbers.groupBy(identity).mapValues(_.size)
    numbers.exists(elem => {
      val dif = k - elem
      if (dif == elem)
        histo.get(dif).exists(x => x > 1)
      else
        histo.contains(dif)
    })
  }

}
