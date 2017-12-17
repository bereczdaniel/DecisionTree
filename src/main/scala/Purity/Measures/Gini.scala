package Purity.Measures

import scala.collection.mutable

object Gini {

  def gini(classCounters: mutable.HashMap[String, Int]): Double = {
    val allInstance = classCounters.values.sum.toDouble
    val fractions = classCounters.map(x => math.pow(x._2 / allInstance,2))
    1.0 - fractions.sum
  }

}
