package test.utils

import instance.{Features, Instance}

import scala.util.Random

object Utils {

  def createTrainingPointsFromMultipleClass(params: Array[(Int, Int, String)]): Array[TestType] =
    params.map(p => createTrainingPointsFromOneClass(p._1, p._2, p._3)).reduce((a,b) => a ++ b)

  def createTrainingPointsFromOneClass(num: Int, upper: Int, label: String): Array[TestType] =
    (for(_ <- 0 until num) yield new TestType(Dummy(Random.nextInt(upper), Random.nextDouble()), label)).toArray
}

case class Dummy(h: Int, w: Double) extends Features{
  override def getValues: Array[Double] = Array[Double](h, w)
}

class TestType(attribute: Dummy, label: String) extends Instance(attribute, label){

}
