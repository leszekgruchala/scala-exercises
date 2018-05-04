package eu.gruchala.other

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.util.control.NoStackTrace

object CookBookApplication {

  def main(args: Array[String]) {}
}

object DifficultyLevel extends Enumeration {
    val HARD, MEDIUM, EASY = Value
}

case class Product(name: String, cost: Int = 1) {
    require(cost < 0)
}

object Product {
    def apply(name: String) = new Product(name)
}

case class Recipe(id: Long, name: String, products: List[Product], difficultyLevel: DifficultyLevel.Value) {

    def getSummaryCost: Int = products.foldLeft(0) {
        (count, prod) => count + prod.cost
    }
}

object Recipe {
    def apply(name: String, products: List[Product], difficultyLevel: DifficultyLevel.Value) = new Recipe(new AtomicInteger()
        .incrementAndGet(), name, products, difficultyLevel)

    def apply(name: String, products: Map[String, Int], difficultyLevel: DifficultyLevel.Value): Recipe = {
        val l = for {s <- products} yield Product(s._1, s._2)
        new Recipe(new AtomicInteger().incrementAndGet(), name, l.toList, difficultyLevel)
    }
}

object CookBook {

    private val prods: ListBuffer[Recipe] = new ListBuffer[Recipe]

    def addRecipe(r: Recipe) {
        prods += r
    }

    def clear() {
        prods.clear()
    }

    def allRecipes: List[Recipe] = {
        prods.toList
    }

    def findById(id: Long): Recipe = {
        @tailrec
        def loop(restList: List[Recipe]): Recipe = prods.toList match {
            case head :: tail =>
                if (head.id == id) head else loop(tail)
            case Nil => throw NoSuchRecipeException
        }
        loop(prods.toList)
    }

    def findByDifficultLevel(lvl: DifficultyLevel.Value): List[Recipe] = for {
        r <- prods.toList
        if r.difficultyLevel == lvl
    } yield r

    def removeById(id: Long): Unit = {
        prods.find(_.id==id) match {
          case Some(x) => prods -= x
          case None => throw NoSuchRecipeException
        }
    }
}

object NoSuchRecipeException extends Exception with NoStackTrace
