package Utils

import Purity.Measures.Gini


object Utils {

  def bestSplit[T: Ordering](instances: Array[(T, String)]): (T, Double) = {

    var bestBoundary = instances.head._1
    var bestGini = 1.0

    for(instance <- instances){
      val (leftLeaf, rightLeaf) = createState(instances, instance._1)
      val leftGini = Gini.gini(leftLeaf)
      val rightGini = Gini.gini(rightLeaf)

      val comboGini = (leftGini + rightGini) / 2
      if(comboGini <= bestGini){
        bestBoundary = instance._1
        bestGini = comboGini
      }
    }
    (bestBoundary, bestGini)
  }

  def createState[T: Ordering]( instances: Array[(T, String)],
                                boundary: T): (Map[String, Int], Map[String, Int]) = {
    import scala.math.Ordering.Implicits._
    ( instances.filter(_._1 >= boundary).groupBy(_._2).map(x => (x._1, x._2.length)),
      instances.filterNot(_._1 >= boundary).groupBy(_._2).map(x => (x._1, x._2.length)))
  }


}
