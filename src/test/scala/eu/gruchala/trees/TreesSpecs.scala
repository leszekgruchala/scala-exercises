package eu.gruchala.trees

import eu.gruchala.BaseSpec
import eu.gruchala.trees.TreesOps._

class TreesSpecs extends BaseSpec {

  val binaryTree = BinaryTreeNode(value = 9,
    left = Some(value = BinaryTreeNode(value = 8,
      left = Some(value = BinaryTreeNode(value = 5, left = None, right = None)),
      right = Some(BinaryTreeNode(value = 6, left = None, right = None)))),
    right = Some(value = BinaryTreeNode(value = 11,
      left = None,
      right = Some(value = BinaryTreeNode(value = 20, left = None, right = None)))))

  "A binary tree" should {
    "return correct order of nodes" when {
      "given in-order traversal" in {
        inOrderTraversal(binaryTree) shouldEqual Vector(5, 8, 6, 9, 11, 20)
      }
      "given pre-order traversal" in {
        preOrderTraversal(binaryTree) shouldEqual Vector(9, 8, 5, 6, 11, 20)
      }
      "given post-order traversal" in {
        postOrderTraversal(binaryTree) shouldEqual Vector(5, 6, 8, 11, 20, 9)
      }
    }

    "recognise if tree is full" in {
      isFull(binaryTree) shouldEqual false
      isFull(
        BinaryTreeNode(value = 9,
          left = Some(value = BinaryTreeNode(value = 8,
            left = Some(value = BinaryTreeNode(value = 5, left = None, right = None)),
            right = Some(BinaryTreeNode(value = 6, left = None, right = None)))),
          right = Some(value = BinaryTreeNode(value = 11,
            left = Some(BinaryTreeNode(value = 13, left = None, right = None)),
            right = Some(value = BinaryTreeNode(value = 20, left = None, right = None)))))
      ) shouldEqual true
    }

    "recognise if tree is complete" in {
      isComplete(binaryTree) shouldEqual false
      isComplete(
        BinaryTreeNode(value = 9,
          left = Some(value = BinaryTreeNode(value = 8,
            left = Some(value = BinaryTreeNode(value = 5, left = None, right = None)),
            right = Some(BinaryTreeNode(value = 6, left = None, right = None)))),
          right = Some(value = BinaryTreeNode(value = 11,
            left = Some(value = BinaryTreeNode(value = 20, left = None, right = None)),
            right = None)))
      ) shouldEqual true
    }

    "recognise if tree is perfect" in {
      isPerfect(binaryTree) shouldEqual false
      isPerfect(
        BinaryTreeNode(value = 9,
          left = Some(value = BinaryTreeNode(value = 8,
            left = Some(value = BinaryTreeNode(value = 5, left = None, right = None)),
            right = Some(BinaryTreeNode(6, None, None)))),
          right = Some(value = BinaryTreeNode(value = 11,
            left = Some(value = BinaryTreeNode(value = 13, left = None, right = None)),
            right = Some(value = BinaryTreeNode(value = 20, left = None, right = None)))))
      ) shouldEqual true
    }

    "recognise if tree is balanced" in {
      isBalanced(binaryTree) shouldEqual true
      isBalanced(
        BinaryTreeNode(value = 9,
          left = Some(value = BinaryTreeNode(value = 8,
            left = None,
            right = Some(BinaryTreeNode(6, None, None)))),
          right = Some(value = BinaryTreeNode(value = 11,
            left = None,
            right = Some(value = BinaryTreeNode(value = 20, left = None, right = None)))))
      ) shouldEqual false
    }
  }
}
