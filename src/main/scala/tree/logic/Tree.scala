package tree.logic

import instance.{Features, Instance}
import utils.Measures
import utils.Utils.{createRule, createState}

sealed abstract class Tree(left: Tree, right: Tree) {
  def predict(instance: Features): String

  def split(): Tree

  def countLeafs(): Int

  def depth(): Int

  def impurity(): Double

  def worstImpurity(): Double

  def bestImpurityReduction(): Double

  def splitWorst(): Tree

  val state: Map[String, Int]
}

case class Node(var left: Tree,
                var right: Tree,
                rule: Features => Boolean, state: Map[String, Int]) extends Tree(left, right) {

  override def predict(instance: Features): String = {
    if (rule(instance)) {
      left.predict(instance)
    }
    else {
      right.predict(instance)
    }
  }

  override def split(): Tree = {
    left = left.split()
    right = right.split()
    this
  }

  override def countLeafs(): Int =
    left.countLeafs() + right.countLeafs()

  lazy val impurity: Double =
    Measures.gini(state)

  //TODO replace with purity gain (once it's implemented)
  override def splitWorst(): Tree = {
    if(left.bestImpurityReduction() > right.bestImpurityReduction())
      left = left.splitWorst()
    else
      right = right.splitWorst()
    this
  }

  override def worstImpurity(): Double =
    math.max(left.worstImpurity(), right.worstImpurity())

  override def depth(): Int =
    math.max(left.depth(), right.depth()) + 1

  override def bestImpurityReduction(): Double =
    math.max(left.bestImpurityReduction(), right.bestImpurityReduction())
}


case class Leaf[T <: Instance](maxLeafSize: Int, maxImpurity: Double,
                leafInstances: Array[T]) extends Tree(null, null) {

  lazy val state: Map[String, Int] = createState(leafInstances)

  lazy val impurity: Double = Measures.gini(state)

  lazy val potentialRule: Features => Boolean = createRule(leafInstances)

  override def predict(instance: Features): String = {
    state.maxBy(_._2)._1
  }



  override def split(): Tree = {
    if ((maxImpurity < impurity || maxLeafSize < leafInstances.length) && state.size > 1) {
      val newNode = Node(
        Leaf(maxLeafSize, maxImpurity, leafInstances.filter(instance => potentialRule(instance.getFeatures))),
        Leaf(maxLeafSize, maxImpurity, leafInstances.filterNot(instance => potentialRule(instance.getFeatures))),
        potentialRule, state)
      newNode
    }
    else {
      this
    }
  }

  override def countLeafs(): Int = 1

  override def splitWorst(): Tree = split()

  override def worstImpurity(): Double = impurity

  override def depth(): Int = 0

  override def bestImpurityReduction(): Double = {
    if(state.size > 1){
      val potentialLeft = leafInstances.filter(x => potentialRule(x.getFeatures)).map(_.getLabel).groupBy(x => x).map(x => (x._1, x._2.length))
      val potentialRight = leafInstances.filterNot(x => potentialRule(x.getFeatures)).map(_.getLabel).groupBy(x => x).map(x => (x._1, x._2.length))

      impurity -
        (potentialLeft.values.sum * Measures.gini(potentialLeft) +
          potentialRight.values.sum * Measures.gini(potentialRight)) / leafInstances.length
    }
    else {
      0.0
    }
  }
}
