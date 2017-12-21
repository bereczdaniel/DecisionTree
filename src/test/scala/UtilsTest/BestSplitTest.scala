package UtilsTest

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class BestSplitTest extends FlatSpec with PropertyChecks with Matchers{

  "BS" should "give the perfect split" in {
    val instances = Array(
      (1.2, "A"),
      (2.5, "A"),
      (3.6, "A"),
      (4.3, "A"),
      (5.2, "B"),
      (6.1, "B"),
      (7.9, "B"))

    Utils.Utils.bestSplit(instances) shouldBe 5.0
  }

  "BS" should "give the best possible split" in {
    val instances = Array(
      (1.1, "A"),
      (2.2, "A"),
      (3.3, "A"),
      (4.4, "A"),
      (5.5, "B"),
      (6.6, "B"),
      (7.7, "B"),
      (8.8, "A"))

    Utils.Utils.bestSplit(instances) shouldBe 5.0
  }


  "BS" should "give the best possible split with integers" in {
    val instances = Array(
      (1, "A"),
      (2, "A"),
      (3, "A"),
      (4, "A"),
      (5, "B"),
      (6, "B"),
      (7, "B"),
      (8, "A"))

    Utils.Utils.bestSplit(instances) shouldBe 5.0
  }


}
