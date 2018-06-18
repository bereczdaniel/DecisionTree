package tree.classifier

import instance.Instance
import tree.logic.{Leaf, Tree}

import scala.annotation.tailrec

/**
  * Represents the classifier ???
  * @param maxLeafSize: Maximum number of [[Instance]]s in a leaf
  * @param maxImpurity: Maximum impurity in a leaf
  * @param maxDepth: Maximal depth of the tree
  * @param maxLeafNumber: Maximum number of leaves in the tree
  */
class TreeClassifier(maxLeafSize: Int = 1, maxImpurity: Double = 0.0, maxDepth: Int = Int.MaxValue, maxLeafNumber: Int = Int.MaxValue) {


  /**
    * Executes the model training on the given data
    * @param data: Training data
    * @tparam A: Parameter type of the training points, must be a subtype of [[Instance]]
    * @return trained model
    */
  def train[A <: Instance](data: Array[A]): Tree = {
    val base = Leaf(maxLeafSize, maxImpurity, data)

    if (math.pow(maxDepth, 2) > maxLeafNumber)
      trainOneByOne(base, 0)
    else
      trainInner(1, base, 0)
  }

  /**
    * Split the actual tree at every leaf, when the constraints let it, until there is no possible split or max depth is reached
    * @param depth: Current depth of the function / tree
    * @param currentTree: Current version of the model
    * @param prevLeafNumber: Number of leaves in the previous iteration
    * @return trained model
    */
  @tailrec
  private def trainInner(depth: Int, currentTree: Tree, prevLeafNumber: Int): Tree = {
    val currentLeafs = currentTree.countLeafs()

    if (depth == maxDepth || currentLeafs == prevLeafNumber)
      currentTree
    else
      trainInner(depth + 1, currentTree.split(), currentLeafs)
  }

  /**
    * Split the actual trees worst leaf, until there is no possible split or max number of leaves is reached
    * @param currentTree: Current version of the model
    * @param prevLeafNumber: Number of leaves in the previous iteration
    * @return trained model
    */
  //TODO force maxDepth restriction
  @tailrec
  private def trainOneByOne(currentTree: Tree, prevLeafNumber: Int): Tree = {
    val currentLeafs = currentTree.countLeafs()

    if (currentTree.countLeafs() == maxLeafNumber || prevLeafNumber == currentLeafs)
      currentTree
    else
      trainOneByOne(currentTree.splitWorst(), currentLeafs)
  }
}
