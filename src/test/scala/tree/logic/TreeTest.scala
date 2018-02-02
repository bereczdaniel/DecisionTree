package tree.logic

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import test.utils.TestType
import test.utils.Utils._

class TreeTest extends FlatSpec with PropertyChecks with Matchers {


  "Tree operation" should "work" in {
    val maxLeafSize = 10
    val maxImpurity = 0.0

    val dataA = createTrainingPointsFromOneClass(2,10,"A")
    val dataB = createTrainingPointsFromOneClass(4,10,"B")
    val dataA2 = createTrainingPointsFromOneClass(4,10,"A")
    val dataB2 = createTrainingPointsFromOneClass(4,10,"B")


    Leaf(maxLeafSize, maxImpurity, dataA).predict(null) shouldBe "A"
    Leaf(maxLeafSize, maxImpurity, dataA ++ dataB).predict(null) shouldBe "B"
    Leaf(maxLeafSize, maxImpurity, dataA ++ dataB ++ dataA2).predict(null) shouldBe "A"
    Leaf(maxLeafSize, maxImpurity,  dataA ++ dataB ++ dataA2 ++ dataB2).predict(null) shouldBe "B"


  }

  "Split operation" should "work" in {
    val maxLeafSize = 3
    val maxImpurity = 0.0

    val testTree = Leaf(maxLeafSize, maxImpurity, createTrainingPointsFromOneClass(2,10,"A"))


    val splitTree1 = testTree.split()
    splitTree1.isInstanceOf[Leaf[TestType]] shouldBe true


    val testTree2 = Leaf(maxLeafSize, maxImpurity, createTrainingPointsFromMultipleClass(Array((4, 10, "A"), (4, 20, "B"))))

    val splitTree2 = testTree2.split()
    splitTree2.isInstanceOf[Node] shouldBe true

    var impurityTree = Leaf(10, 0.001, createTrainingPointsFromOneClass(4, 20, "A"))

    val impurityTreeSplit = impurityTree.split()
    impurityTreeSplit.isInstanceOf[Leaf[TestType]] shouldBe true

    impurityTree = Leaf(maxLeafSize, maxImpurity, createTrainingPointsFromMultipleClass(Array((4, 10, "A"), (4, 20, "B"))))

    val impurityTreeSplit2 = impurityTree.split()
    impurityTreeSplit2.isInstanceOf[Node] shouldBe true
  }

  "New rules" should "be created" in {
    val maxLeafSize = 3
    val maxImpurity = 0.0
    //TODO figure out how to test
  }

  "CountLeafs" should "give the number of leafs" in {
    val maxLeafSize = 3
    val maxImpurity = 0.0

    val testTree = Leaf(maxLeafSize, maxImpurity, createTrainingPointsFromMultipleClass(Array((3, 10, "A"), (3, 20, "B"))))

    testTree.countLeafs() shouldBe 1

    val firstSplit = testTree.split()

    firstSplit.countLeafs() shouldBe 2

    val secondSplit = firstSplit.split()

    secondSplit.countLeafs() shouldBe 2
  }

  "Gini" should "give the gini index in the given leaf" in {
    val maxLeafSize = 3
    val maxImpurity = 0.0

    val testTree = Leaf(maxLeafSize, maxImpurity, createTrainingPointsFromMultipleClass(Array((3, 10, "A"), (3, 40, "B"))))

    testTree.impurity shouldBe 0.5

    val splitTree = testTree.split()

    splitTree.asInstanceOf[Node].right.asInstanceOf[Leaf[TestType]].impurity shouldBe 0.0
    splitTree.asInstanceOf[Node].left.asInstanceOf[Leaf[TestType]].impurity shouldBe 0.0
  }

  //TODO additional test, to check for the best split
  "Worst split" should "split only the leaf with the worst impurity" in {

  }
}
