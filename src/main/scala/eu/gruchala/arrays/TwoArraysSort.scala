package eu.gruchala.arrays

object TwoArraysSort {

  //arrays can be of different sizes and have overlapping numbers
  def max(k: Int, array1: Array[Int], array2: Array[Int]): Int = {
    val array1Length: Int = array1.length
    val array2Length: Int = array2.length

    @scala.annotation.tailrec
    def loop(arrayIndex1: Int, arrayIndex2: Int, acc: Vector[Int]): Vector[Int] = {

      if (arrayIndex1 < array1Length && arrayIndex2 < array2Length) {
        val a = array1(arrayIndex1)
        val b = array2(arrayIndex2)

        if (a < b) loop(arrayIndex1 + 1, arrayIndex2, acc :+ a)
        else if (a > b) loop(arrayIndex1, arrayIndex2 + 1, acc :+ b)
        else loop(arrayIndex1 + 1, arrayIndex2 + 1, acc :+ a)

      } else if (arrayIndex1 < array1Length && arrayIndex2 == array2Length) {
        loop(arrayIndex1 + 1, arrayIndex2, acc :+ array1(arrayIndex1))
      } else if (arrayIndex1 == array1Length && arrayIndex2 < array2Length) {
        loop(arrayIndex1, arrayIndex2 + 1, acc :+ array2(arrayIndex2))
      } else acc
    }

    val xs = loop(0, 0, Vector.empty)
    xs(xs.size - k)
  }

}
