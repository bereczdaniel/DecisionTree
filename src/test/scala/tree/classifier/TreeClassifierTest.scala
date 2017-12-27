package tree.classifier

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import test.utils.{Dummy, TestType}
import scala.util.Random

class TreeClassifierTest extends FlatSpec with PropertyChecks with Matchers {

  def createTrainingPoints(numA: Int, numB: Int, upperA: Int, upperB: Int): Array[TestType] = {
    val trainA = (for(_ <- 0 until numA) yield new TestType(Dummy(Random.nextInt(upperA), Random.nextDouble()), "A")).toArray
    val trainB = (for(_ <- 0 until numB) yield new TestType(Dummy(Random.nextInt(upperB), Random.nextDouble()), "B")).toArray
    trainA ++ trainB
  }

  "Tree classifier" should "predict the class" in {
    val clf = new TreeClassifier(3, 0.0, 3)

    val train = createTrainingPoints(4,4,10,20)

    val model = clf.train(train)

    model.predict(Dummy(15, 1.2)) shouldBe "B"
  }

  "Tree classifier" should "stop without maxDepth" in {
    val clf = new TreeClassifier(maxImpurity = 0.2, maxLeafSize = 3)

    val train = createTrainingPoints(4,4,10,20)

    val model = clf.train(train)

    model.predict(Dummy(15, 1.2)) shouldBe "B"
  }

  "Tree classifier" should "work without impurity or leaf size" in {
    val clfWithoutImpurity = new TreeClassifier(maxLeafSize = 3)
    val train = createTrainingPoints(4,4,10,20)

    val modelI = clfWithoutImpurity.train(train)

    modelI.predict(Dummy(15, 1.2)) shouldBe "B"

    val clfWithoutLeafSize = new TreeClassifier(maxImpurity = 0.1)

    val modelL = clfWithoutLeafSize.train(train)

    modelL.predict(Dummy(15, 1.2)) shouldBe "B"
  }

}
