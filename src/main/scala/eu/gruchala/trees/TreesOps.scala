package eu.gruchala.trees

import scala.annotation.tailrec

class TreesOps {

  def showGenerations(root: Node): String = {
    @tailrec
    def deepChildren(parents: List[Node], mem: List[List[Node]]): List[List[Node]] = {
      val currentChildren = parents.flatMap(_.children)

      if (currentChildren.isEmpty) (parents :: mem).reverse
      else deepChildren(currentChildren, parents :: mem)
    }
    val gens: List[List[Node]] = deepChildren(List(root), Nil)

    gens.map(_.map(_.value).mkString(" ")).mkString("\n")
  }

}
