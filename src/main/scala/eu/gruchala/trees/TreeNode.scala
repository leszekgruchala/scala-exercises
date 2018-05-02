package eu.gruchala.trees

case class TreeNode(value: String, children: Vector[TreeNode])

case class BinaryTreeNode(value: Int, left: Option[BinaryTreeNode], right: Option[BinaryTreeNode])
