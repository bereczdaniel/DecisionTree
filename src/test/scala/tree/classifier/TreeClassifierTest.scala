package tree.classifier

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import test.utils.{Dummy, TestType}
import scala.util.Random

class TreeClassifierTest extends FlatSpec with PropertyChecks with Matchers {

  "Tree classifier" should "predict the class" in {
    val clf = new TreeClassifier(3, 3)

    val trainA = (for(_ <- 0 until 4) yield new TestType(Dummy(Random.nextInt(10), Random.nextDouble()), "A")).toArray
    val trainB = (for(_ <- 0 until 4) yield new TestType(Dummy(Random.nextInt(20), Random.nextDouble()), "B")).toArray
    val train = trainA ++ trainB

    val model = clf.train(train)

    model.predict(Dummy(15, 1.2)) shouldBe "B"
  }

}
