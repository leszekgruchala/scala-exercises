package eu.gruchala.trees

case class TreeNode(value: String, children: Vector[TreeNode])

sealed abstract class Node[+A]
final case object Leaf extends Node[Nothing]
final case class Branch[A](value: A, left: Node[A], right: Node[A]) extends Node[A]
