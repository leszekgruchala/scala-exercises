package eu.gruchala.trees

import scala.annotation.tailrec

object TreeOps {

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
  def inOrderTraversal[A](node: Node[A]): Vector[A] =
    node match {
      case Leaf => Vector.empty
      case Branch(v, l, r) =>
        (inOrderTraversal(l) :+ v) ++ inOrderTraversal(r)
    }

  //node, left, right
  def preOrderTraversal[A](node: Node[A]): Vector[A] =
    node match {
      case Leaf => Vector.empty
      case Branch(v, l, r) =>
        (v +: preOrderTraversal(l)) ++ preOrderTraversal(r)
    }

  //left, right, node
  def postOrderTraversal[A](node: Node[A]): Vector[A] =
    node match {
      case Leaf => Vector.empty
      case Branch(v, l, r) =>
        postOrderTraversal(l) ++ postOrderTraversal(r) :+ v
    }

  //All levels fully filled, except the last one - if last is one not fully filled, then can be only on the left
  def isComplete[A](treeNode: Node[A]): Boolean = {
    treeNode match {
      case Leaf => true
      case Branch(_, Leaf, Leaf)  => true
      case Branch(_, Branch(_, Leaf, Leaf), Leaf) =>
        true
      case Branch(_, l@Branch(_, _, _), r@Branch(_, _, _)) =>
        isComplete(l) && isComplete(r)
      case _ => false
    }
  }

  //Either zero or two children
  def isFull[A](treeNode: Node[A]): Boolean = {
    treeNode match {
      case Leaf => true
      case Branch(_, Leaf, Leaf) => true
      case Branch(_, left@Branch(_, _, _), right@Branch(_, _, _)) =>
        isComplete(left) && isComplete(right)
      case _ => false
    }
  }

  //All nodes have two children and all leaves are at same level.
  def isPerfect[A](treeNode: Node[A]): Boolean = {
    def leftMostDepth(treeNode: Node[A], acc: Int): Int = {
      treeNode match {
        case Leaf => acc
        case Branch(_, left, _) =>
          leftMostDepth(left, acc + 1)
      }
    }
    //Look somewhere for expected level for all paths
    val depth = leftMostDepth(treeNode, 0)

    def loop(treeNode: Node[A], maxDepth: Int, depth: Int): Boolean = {
      treeNode match {
        case Leaf => true
        case Branch(_, Leaf, Leaf) =>
          //at the final leaf, check if depth is the same as max one
          maxDepth == depth + 1
        case Branch(_, l@Branch(_, _, _), r@Branch(_, _, _)) =>
          loop(l, maxDepth, depth + 1) && loop(r, maxDepth, depth + 1)
        case _ => false
      }
    }

    loop(treeNode, depth, 0)
  }

  //Like AVL tree, the height difference between lefts and rights is 0 or 1 level
  def isHeightBalanced[A](treeNode: Node[A]): Boolean = {
    //Height as longest path
    def height(treeNode: Node[A]): Int = {
      treeNode match {
        case Leaf => 0
        case Branch(_, left, right) =>
          1 + math.max(height(left), height(right))
      }
    }

    treeNode match {
      case Leaf => true
      case Branch(v, left, right) =>
        val lefts = height(left)
        val rights = height(right)
        println(s"for node: $v lefts: $lefts rights: $rights")
        if (math.abs(lefts - rights) <= 1 &&
          isHeightBalanced(left) &&
          isHeightBalanced(right)) true else false
    }
  }
}
