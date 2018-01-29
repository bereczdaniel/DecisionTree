package tree.logic

import utils.Utils.bestSplit
import instance.{Features, Instance}
import utils.Measures

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed abstract class Tree(left: Tree, right: Tree) {
  def predict(instance: Features): String
  def insert(instance: Instance): Unit
  def split(): Tree
  def countLeafs(): Int
}

case class Node(var left: Tree,
                var right: Tree,
                rule: Features => Boolean) extends Tree(left, right){
  override def predict(instance: Features): String = {
    if(rule(instance)){
      left.predict(instance)
    }
    else{
      right.predict(instance)
    }
  }

  override def insert(instance: Instance): Unit = {
    if(rule(instance.getFeatures)){
      left.insert(instance)
    }
    else {
      right.insert(instance)
    }
  }

  override def split(): Tree = {
    left = left.split()
    right = right.split()
    this
  }

  override def countLeafs(): Int =
    left.countLeafs() + right.countLeafs()

}


case class Leaf(maxLeafSize: Int, maxImpurity: Double) extends Tree(null, null) {

  val leafInstances = new ArrayBuffer[Instance]()
  val state = new mutable.HashMap[String, Int]()
  def gini(): Double = Measures.gini(state)

  override def predict(instance: Features): String = {
    state.maxBy(_._2)._1
  }

  override def insert(instance: Instance): Unit = {
    leafInstances += instance
    state.update(instance.getLabel, state.getOrElse(instance.getLabel, 0) + 1)
  }

  def createRule(): Features => Boolean = {
    val a = leafInstances.map(_.getFeatures.getValues)
    val b = (for(i <- a.head.indices)
      yield (for(j <- a.indices)
        yield a(j)(i)).toArray.zip(leafInstances.map(_.getLabel)))
      .toArray

    val boundaries = (for(i <- b.indices) yield (bestSplit(b(i)), i)).toArray.minBy(_._1._2)

    {f => f.getValues(boundaries._2) >= (boundaries._1._1 * 0.99)}
  }

  override def split(): Tree = {

    if((maxImpurity < gini || maxLeafSize < leafInstances.size) && state.size > 1){
      val newRule: Features => Boolean = createRule()
      val newNode = Node(
        Leaf(maxLeafSize, maxImpurity),
        Leaf(maxLeafSize, maxImpurity),
        newRule)
      for(instance <- leafInstances){
        newNode.insert(instance)
      }
      newNode
    }
    else {
      this
    }
  }

  override def countLeafs(): Int = 1
}
