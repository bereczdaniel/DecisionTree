package tree.classifier

import instance.Instance
import tree.logic.{Leaf, Tree}

class TreeClassifier(maxLeafSize: Int = 1, maxImpurity: Double = 0.0, maxDepth: Int = Int.MaxValue) {

  def train[A <: Instance](data: Array[A]): Tree = {
    val base = Leaf(maxLeafSize, maxImpurity)

    for(instance <- data){
      base.insert(instance)
    }

    trainInner(1, base, 0)
  }

  def trainInner(depth: Int, currentTree: Tree, prevLeafNumber: Int): Tree = {
    val currentLeafs = currentTree.countLeafs()

    if(depth == maxDepth || currentLeafs == prevLeafNumber){
      currentTree
    }
    else {
      trainInner(depth + 1, currentTree.split(), currentLeafs)
    }
  }
}
