package utils

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import utils.Utils.trainTestSplit

class trainTestSplitTest extends FlatSpec with PropertyChecks with Matchers{

  "Splitter" should "shuffle and split the data into train and test" in{
    val data = Array(1,2,3,4,5,6,7,8,9,0)
    val (train, test) = trainTestSplit(data, 0.7)

    train.length shouldBe 7
    test.length shouldBe 3

    val data2 = Array(1,2,3,4,5,6,7,8,9)
    val (train2, test2) = trainTestSplit(data2, 0.5)

    train2.length shouldBe 5
    test2.length shouldBe 4
  }
}
