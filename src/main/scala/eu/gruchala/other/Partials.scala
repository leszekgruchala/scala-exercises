package eu.gruchala.other

object Partials {

  val factorialPartialFunction: PartialFunction[Int, Int] = {
    case 0 => 1
    case a: Int if a > 0 => factorialPartialFunction(a - 1) * a
  }

  def evenOddChecker(l: List[Int]) {
    val evenPartial: PartialFunction[Int, Unit] = {
      case a: Int if a % 2 == 0 => println(s"it's even! $a")
    }

    val oddPartial: PartialFunction[Int, Unit] = {
      case a: Int if a % 2 == 1 => println(s"it's odd! $a")
    }

    val z = evenPartial orElse oddPartial
    l foreach z
  }

  def evenOddIncrementChecker(l: List[Int]) {
    val evenPartial: PartialFunction[Int, Int] = {
      case a: Int if a % 2 == 0 => a + 3
    }

    val oddPartial: PartialFunction[Int, Int] = {
      case a: Int if a % 2 == 1 => a + 5
    }

    val z = evenPartial orElse oddPartial
    val valueCheckerPass: PartialFunction[Int, List[Int]] = {
      case a: Int if a >= 10 => List(a)
    }
    val valueCheckerFail: PartialFunction[Int, List[Int]] = {
      case a: Int if a < 10 => Nil
    }
    val checker = valueCheckerPass orElse valueCheckerFail
    val resultPartial = z andThen checker
    val listOfList = for (i <- l) yield resultPartial(i)
    println(listOfList flatten)
  }

  def main(args: Array[String]) {
    //        factorialPartialFunction(-1)
    println(if (factorialPartialFunction.isDefinedAt(-1)) {
      factorialPartialFunction(-1)
    })

    //        evenOddChecker(List (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0))
    evenOddIncrementChecker(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0))
  }
}
