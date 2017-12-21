package Tree

import Instance.Instance

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class Tree[X, A <: Instance[X]](left: Tree[X, A], right: Tree[X, A]) {
  def predict(instance: X): String
  def insert(instance: A): Unit
  def split(): Tree[X, A]

}

class Node[X, A <: Instance[X]](var left: Tree[X, A],
                                      var right: Tree[X, A],
                                      rule: X => Boolean) extends Tree(left, right){
  override def predict(instance: X): String = {
    if(rule(instance)){
      left.predict(instance)
    }
    else{
      right.predict(instance)
    }
  }

  override def insert(instance: A): Unit = {
    if(rule(instance.getAttributes)){
      left.insert(instance)
    }
    else {
      right.insert(instance)
    }
  }

  override def split(): Tree[X, A] = {
    left = left.split()
    right = right.split()
    this
  }
}

class Leaf[X, A <: Instance[X]](minSplit: Int) extends Tree[X, A](null, null) {

  val leafInstances = new ArrayBuffer[A]()
  val state = new mutable.HashMap[String, Int]()

  override def predict(instance: X): String = {
    state.maxBy(_._2)._1
  }

  override def insert(instance: A): Unit = {
    leafInstances += instance
    state.update(instance.getLabel, state.getOrElse(instance.getLabel, 0) + 1)
  }

  def createRule(): X => Boolean = {_ => true}

  override def split(): Tree[X, A] = {
    if(minSplit > leafInstances.size){
      this
    }
    else {
      val newRule: X => Boolean = createRule()
      val newNode = new Node(
        new Leaf[X, A](minSplit),
        new Leaf[X, A](minSplit),
        newRule)
      for(instance <- leafInstances){
        newNode.insert(instance)
      }
      newNode
    }
  }
  
  
}
