package tree.classifier

import instance.Instance
import tree.logic.{Leaf, Node, Tree}

class TreeClassifier(minSplit: Int, maxDepth: Int) {

  def train[A <: Instance](data: Array[A]): Tree = {
    val base = Leaf(minSplit)

    for(instance <- data){
      base.insert(instance)
    }

    trainInner(1, base)
  }

  def trainInner(depth: Int, currentTree: Tree): Tree = {
    if(depth == maxDepth){
      currentTree
    }
    else {
      trainInner(depth + 1, currentTree.split())
    }
  }
}
