package eu.gruchala.trees

import scala.annotation.tailrec

object TreesOps {

  def showGenerations(root: TreeNode): String = {
    @tailrec
    def loop(parents: List[TreeNode], mem: List[List[TreeNode]]): List[List[TreeNode]] = {
      val currentChildren = parents.flatMap(_.children)

      if (currentChildren.isEmpty) (parents :: mem).reverse
      else loop(currentChildren, parents :: mem)
    }

    loop(List(root), Nil)
      .map(_.map(_.value).mkString(" ")).mkString("\n")
  }

  //left, node, right
  def inOrderTraversal(node: BinaryTreeNode): Vector[Int] =
    (orEmpty(node.left, inOrderTraversal) :+ node.value) ++ orEmpty(node.right, inOrderTraversal)

  //node, left, right
  def preOrderTraversal(node: BinaryTreeNode): Vector[Int] =
    (node.value +: orEmpty(node.left, preOrderTraversal)) ++ orEmpty(node.right, preOrderTraversal)

  //left, right, node
  def postOrderTraversal(node: BinaryTreeNode): Vector[Int] =
    (orEmpty(node.left, postOrderTraversal) ++ orEmpty(node.right, inOrderTraversal)) :+ node.value

  private def orEmpty(node: Option[BinaryTreeNode], traversal: BinaryTreeNode => Vector[Int]): Vector[Int] =
    node.map(traversal).getOrElse(Vector.empty)

  //All levels fully filled, except the last one - if last is one not fully filled, then can be only on the left
  def isComplete(treeNode: BinaryTreeNode): Boolean = {
    val childrenNo = Seq(treeNode.left, treeNode.right).flatten.size
    (childrenNo == 0 || childrenNo == 2 || (childrenNo == 1 && treeNode.right.isEmpty)) &&
      treeNode.left.forall(isComplete) &&
      treeNode.right.forall(isComplete)
  }

  //Either zero or two children
  def isFull(treeNode: BinaryTreeNode): Boolean = {
    val childrenNo = Seq(treeNode.left, treeNode.right).flatten.size
    (childrenNo == 0 || childrenNo == 2) && treeNode.left.forall(isFull) && treeNode.right.forall(isFull)
  }

  //Both full and complete, and all nodes have the same levels
  def isPerfect(treeNode: BinaryTreeNode): Boolean = {
    val hasSameNoLevels = {
      val (lefts, rights) = countLevels(treeNode)
      math.abs(lefts - rights) == 0
    }

    val childrenNo = Seq(treeNode.left, treeNode.right).flatten.size
    hasSameNoLevels && (childrenNo == 0 || childrenNo == 2) && treeNode.left.forall(isPerfect) && treeNode.right.forall(isPerfect)
  }

  //Like AVL tree, the height difference between lefts and rights is 0 or 1 level
  def isBalanced(treeNode: BinaryTreeNode): Boolean = {
    val (lefts, rights) = countLevels(treeNode)
    val result = math.abs(lefts - rights)
    result == 0 || result == 1
  }

  private def countLevels(treeNode: BinaryTreeNode): (Int, Int) = {
    def sum(tuples: (Int, Int)*): (Int, Int) = tuples.reduceLeft[(Int, Int)] { case ((a1, a2), (b1, b2)) =>
      (a1 + b1) -> (a2 + b2)
    }
    def counter(nodes: Option[BinaryTreeNode]*): Int = nodes.flatten.size

    def loop(n: Option[BinaryTreeNode], acc: (Int, Int)): (Int, Int) = {
      n.map { n1 =>
        (n1.left, n1.right) match {
          case (None, None) => acc
          case (Some(l), Some(r)) =>
            val currentAcc = sum(acc, 1 -> 1)
            sum(loop(Some(l), currentAcc), loop(Some(r), currentAcc))
          case (Some(l), None) =>
            loop(Some(l), sum(acc, 1 -> 0))
          case (None, Some(r)) =>
            loop(Some(r), sum(acc, 0 -> 1))
        }
      }.getOrElse(acc)
    }

    val left = treeNode.left
    val right = treeNode.right
    sum(loop(left, counter(left) -> 0), loop(right, 0 -> counter(right)))
  }
}
