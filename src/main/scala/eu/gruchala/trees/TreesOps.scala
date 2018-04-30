package eu.gruchala.trees

import scala.annotation.tailrec

class TreesOps {

  def showGenerations(root: Node): String = {
    @tailrec
    def loop(parents: List[Node], mem: List[List[Node]]): List[List[Node]] = {
      val currentChildren = parents.flatMap(_.children)

      if (currentChildren.isEmpty) (parents :: mem).reverse
      else loop(currentChildren, parents :: mem)
    }

    loop(List(root), Nil)
      .map(_.map(_.value).mkString(" ")).mkString("\n")
  }

}
