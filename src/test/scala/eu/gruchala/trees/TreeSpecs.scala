package eu.gruchala.trees

import eu.gruchala.BaseSpec
import eu.gruchala.trees.TreeOps._

class TreeSpecs extends BaseSpec {
  
  /*
  *         9
  *       /  \
  *      8    11
  *    /  \     \
  *   5    6     20
  *
  */
  val binaryTree = Branch(9,
    left = Branch(
      value = 8,
      left = Branch(value = 5, left = Leaf, right = Leaf),
      right = Branch(value = 6, left = Leaf, right = Leaf)
    ),
    right = Branch(
      value = 11,
      left = Leaf,
      right = Branch(value = 20, left = Leaf, right = Leaf)
    )
  )

  "A binary tree" should {
    "return correct order of nodes" when {
      "given in-order traversal" in {
        inOrderTraversal(binaryTree) shouldEqual Vector(5, 8, 6, 9, 11, 20)
      }
      "given pre-order traversal" in {
        preOrderTraversal(binaryTree) shouldEqual Vector(9, 8, 5, 6, 11, 20)
      }
      "given post-order traversal" in {
        postOrderTraversal(binaryTree) shouldEqual Vector(5, 6, 8, 20, 11, 9)
      }
    }

    "recognise if tree is full" in {
      isFull(Branch(value = 9, left = Leaf, right = Leaf)) shouldEqual true
      isFull(Branch(value = 9, left = Branch(8, Leaf, Leaf), right = Leaf)) shouldEqual false
      isFull(Branch(value = 9, left = Leaf, right = Branch(10, Leaf, Leaf))) shouldEqual false
      isFull(binaryTree) shouldEqual false
      isFull(
        Branch(value = 9,
          left = Branch(value = 8,
            left = Branch(value = 5, left = Leaf, right = Leaf),
            right = Branch(value = 6, left = Leaf, right = Leaf)
          ),
          right = Branch(value = 11,
            left = Branch(value = 13, left = Leaf, right = Leaf),
            right = Branch(value = 20, left = Leaf, right = Leaf)
          )
        )
      ) shouldEqual true
    }

    "recognise if tree is complete" in {
      isComplete(Branch(value = 9, left = Leaf, right = Leaf)) shouldEqual true
      isComplete(Branch(value = 9, left = Branch(8, Leaf, Leaf), right = Leaf)) shouldEqual true
      isComplete(Branch(value = 9, left = Leaf, right = Branch(10, Leaf, Leaf))) shouldEqual false
      isComplete(Branch(value = 9, left = Leaf, right = Branch(12, Branch(11, Leaf, Leaf), Leaf))) shouldEqual false
      isComplete(Branch(value = 9, left = Branch(5, Branch(3, Leaf, Leaf), Leaf), right = Leaf)) shouldEqual false
      isComplete(Branch(value = 9,
        left = Branch(5, Branch(3, Leaf, Leaf), Branch(4, Leaf, Leaf)),
        right = Leaf)) shouldEqual false
      isComplete(Branch(value = 9,
        left = Branch(5, Branch(3, Leaf, Leaf), Branch(4, Leaf, Leaf)),
        right = Branch(6, Leaf, Leaf))) shouldEqual true
      isComplete(binaryTree) shouldEqual false
      isComplete(
        Branch(value = 9,
          left = Branch(value = 8,
            left = Branch(value = 5, left = Leaf, right = Leaf),
            right = Branch(value = 6, left = Leaf, right = Leaf)
          ),
          right = Branch(value = 11,
            left = Branch(value = 20, left = Leaf, right = Leaf),
            right = Leaf
          )
      )) shouldEqual true
    }

    "recognise if tree is perfect" in {
      isPerfect(Branch(value = 9, left = Leaf, right = Leaf)) shouldEqual true
      isPerfect(Branch(value = 9, left = Branch(8, Leaf, Leaf), right = Leaf)) shouldEqual false
      isPerfect(Branch(value = 9, left = Leaf, right = Branch(10, Leaf, Leaf))) shouldEqual false
      isPerfect(Branch(value = 9, left = Leaf, right = Branch(12, Branch(11, Leaf, Leaf), Leaf))) shouldEqual false
      isPerfect(binaryTree) shouldEqual false
      isPerfect(
        Branch(value = 9,
          left = Branch(value = 8,
            left = Branch(value = 5, left = Leaf, right = Leaf),
            right = Branch(value = 6, left = Leaf, right = Leaf)
          ),
          right = Branch(value = 11,
            left = Branch(value = 13, left = Leaf, right = Leaf),
            right = Branch(value = 20, left = Leaf, right = Leaf)
          )
        )
      ) shouldEqual true
    }

    "recognise if tree is height balanced" in {
      isHeightBalanced(Branch(value = 9, left = Leaf, right = Leaf)) shouldEqual true
      isHeightBalanced(Branch(value = 9, left = Branch(8, Leaf, Leaf), right = Leaf)) shouldEqual true
      isHeightBalanced(Branch(value = 9, left = Leaf, right = Branch(10, Leaf, Leaf))) shouldEqual true
      isHeightBalanced(Branch(value = 9, left = Leaf, right = Branch(12, Branch(11, Leaf, Leaf), Leaf))) shouldEqual false
      isHeightBalanced(Branch(value = 9, left = Branch(8, Branch(5, Leaf, Leaf), Leaf), right = Leaf)) shouldEqual false
      isHeightBalanced(binaryTree) shouldEqual true
      isHeightBalanced(
        Branch(value = 9,
          left = Branch(value = 8,
            left = Leaf,
            right = Branch(6, Leaf, Leaf)
          ),
          right = Branch(value = 11,
            left = Leaf,
            right = Branch(value = 20, left = Leaf, right = Leaf)
          )
        )
      ) shouldEqual true
    }
  }
}
