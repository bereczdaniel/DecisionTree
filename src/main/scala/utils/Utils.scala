package utils

import instance.{Features, Instance}


object Utils {

  def bestSplit(instances: Array[(Double, String)]): (Double, Double) = {

    var bestBoundary = instances.head._1
    var bestGini = 1.0

    for(instance <- instances){
      val (leftLeaf, rightLeaf) = splitInstances(instances, instance._1)
      val leftGini = Measures.gini(leftLeaf)
      val rightGini = Measures.gini(rightLeaf)

      val comboGini = (leftGini * leftLeaf.values.sum + rightGini * rightLeaf.values.sum) / instances.length
      if(comboGini <= bestGini){
        bestBoundary = instance._1
        bestGini = comboGini
      }
    }

    bestBoundary = (bestBoundary + instances.filterNot(_._1>= bestBoundary).maxBy(_._1)._1) / 2.0
    (bestBoundary, bestGini)
  }

  def splitInstances[T: Ordering](instances: Array[(T, String)],
                                  boundary: T): (Map[String, Int], Map[String, Int]) = {
    import scala.math.Ordering.Implicits._
    ( instances.filter(_._1 >= boundary).groupBy(_._2).map(x => (x._1, x._2.length)),
      instances.filterNot(_._1 >= boundary).groupBy(_._2).map(x => (x._1, x._2.length)))
  }

  def createState[T <: Instance](leafInstances: Array[T]): Map[String, Int] =
    leafInstances
      .map(_.getLabel)
      .groupBy(x => x)
      .map(x => (x._1, x._2.length))

  def createRule[T <: Instance](leafInstances: Array[T]): Features => Boolean = {
    val featuresRowWise = leafInstances.map(_.getFeatures.getValues)
    val featuresColumnWise = (for (i <- featuresRowWise.head.indices)
      yield (for (j <- featuresRowWise.indices)
        yield featuresRowWise(j)(i)).toArray.zip(leafInstances.map(_.getLabel)))
      .toArray

    val boundaries = (for (i <- featuresColumnWise.indices) yield (bestSplit(featuresColumnWise(i)), i)).toArray.minBy(_._1._2)

    { f => f.getValues(boundaries._2) >= boundaries._1._1 }
  }

  def trainTestSplit[T](data: Array[T], trainPercentage: Double, shuffle: Boolean = true): (List[T], List[T]) = {
    val shuffled =
      if(shuffle)
        scala.util.Random.shuffle(data.toList)
      else
        data.toList

    (shuffled.take(math.round(data.length * trainPercentage).toInt),
      shuffled.takeRight(data.length - math.round(data.length * trainPercentage).toInt))
  }

}
