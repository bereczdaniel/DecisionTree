package utils

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Metrics._

class MetricsTest extends FlatSpec with PropertyChecks with Matchers {

  "Metrics" should "give the correct computation" in {
    val predictions: Array[Int] = Array(0,1,1,1,0,0,1)
    val y: Array[Int]           = Array(0,1,0,1,1,0,0)

    truePositive(predictions, y) shouldBe 2
    falsePositive(predictions, y) shouldBe 2
    accuracy(predictions, y) shouldBe (4.0/7.0)
    precision(predictions, y) shouldBe (1.0/2.0)
    recall(predictions, y) shouldBe (2.0/3.0)
    F1Score(predictions, y) shouldBe (4.0/7.0)

  }

}
