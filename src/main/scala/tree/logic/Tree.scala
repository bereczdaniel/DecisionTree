package tree.logic

import utils.Utils.bestSplit
import instance.{Features, Instance}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class Tree(left: Tree, right: Tree) {
  def predict(instance: Features): String
  def insert(instance: Instance): Unit
  def split(): Tree

}

class Node(var left: Tree,
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
}

class Leaf(minSplit: Int) extends Tree(null, null) {

  val leafInstances = new ArrayBuffer[Instance]()
  val state = new mutable.HashMap[String, Int]()

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

    val boundaries = (for(i <- b.indices) yield (bestSplit(b(i)), i)).toArray.maxBy(_._1._2)

    {f => f.getValues(boundaries._2) >= boundaries._1._1}
  }

  override def split(): Tree = {
    if(minSplit > leafInstances.size || state.size == 1){
      this
    }
    else {
      val newRule: Features => Boolean = createRule()
      val newNode = new Node(
        new Leaf(minSplit),
        new Leaf(minSplit),
        newRule)
      for(instance <- leafInstances){
        newNode.insert(instance)
      }
      newNode
    }
  }
  
  
}
