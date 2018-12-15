package eu.gruchala.trees

case class TreeNode(value: String, children: Vector[TreeNode])

//FIXME remove
case class BinaryTreeNode(value: Int, left: Option[BinaryTreeNode], right: Option[BinaryTreeNode])
//FIXME use
sealed abstract class Node[+A]
final case object Leaf extends Node[Nothing]
final case class Branch[A](value: A, left: Node[A], right: Node[A]) extends Node[A]
