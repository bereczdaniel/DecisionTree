package tree.classifier

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import test.utils.Dummy
import test.utils.Utils.createTrainingPointsFromMultipleClass

class TreeClassifierTest extends FlatSpec with PropertyChecks with Matchers {

  "Tree classifier" should "predict the class" in {
    val clf = new TreeClassifier(3, 0.0, 3)

    val train = createTrainingPointsFromMultipleClass(Array((4, 10, "A"), (4, 20, "B")))

    val model = clf.train(train)

    model.predict(Dummy(15, 1.2)) shouldBe "B"
  }

  "Tree classifier" should "stop without maxDepth" in {
    val clf = new TreeClassifier(maxImpurity = 0.2, maxLeafSize = 3)

    val train = createTrainingPointsFromMultipleClass(Array((4, 10, "A"), (4, 20, "B")))

    val model = clf.train(train)

    model.predict(Dummy(15, 1.2)) shouldBe "B"
  }

  "Tree classifier" should "work without impurity or leaf size" in {
    val clfWithoutImpurity = new TreeClassifier(maxLeafSize = 3)
    val train = createTrainingPointsFromMultipleClass(Array((4, 10, "A"), (4, 20, "B")))

    val modelI = clfWithoutImpurity.train(train)

    modelI.predict(Dummy(15, 1.2)) shouldBe "B"

    val clfWithoutLeafSize = new TreeClassifier(maxImpurity = 0.1)

    val modelL = clfWithoutLeafSize.train(train)

    modelL.predict(Dummy(15, 1.2)) shouldBe "B"
  }

}
