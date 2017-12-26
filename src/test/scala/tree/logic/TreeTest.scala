package tree.logic

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import test.utils.{Dummy, TestType}

class TreeTest extends FlatSpec with PropertyChecks with Matchers {


  "Tree operation" should "work" in {
    val minSplit = 10


    val testTree = Node(
      Node(
        Leaf(minSplit),
        Leaf(minSplit),
        { _ => true }
      ),
      Node(
        Leaf(minSplit),
        Leaf(minSplit),
        { _ => true }
      ),
      { _ => true }
    )

    val dummyInstance1 = new TestType(Dummy(1, 1.0), "A")
    val dummyInstance2 = new TestType(Dummy(2, 1.25), "A")
    val dummyInstance3 = new TestType(Dummy(3, 1.5), "B")
    val dummyInstance4 = new TestType(Dummy(4, 1.75), "B")
    val dummyInstance5 = new TestType(Dummy(5, 2.0), "B")

    testTree.insert(dummyInstance1)
    testTree.insert(dummyInstance2)
    testTree.predict(Dummy(6, 2.25)) shouldBe "A"

    testTree.insert(dummyInstance3)
    testTree.predict(Dummy(6, 2.25)) shouldBe "A"

    testTree.insert(dummyInstance4)
    testTree.insert(dummyInstance5)

    testTree.predict(Dummy(6, 2.25)) shouldBe "B"
  }

  "Split operation" should "work" in {
    val minSplit = 3

    val testTree = Leaf(minSplit)

    val dummyInstance1 = new TestType(Dummy(1, 1.0), "A")
    val dummyInstance2 = new TestType(Dummy(2, 1.25), "A")
    val dummyInstance3 = new TestType(Dummy(3, 1.5), "B")
    val dummyInstance4 = new TestType(Dummy(4, 1.75), "B")
    val dummyInstance5 = new TestType(Dummy(5, 2.0), "B")

    testTree.insert(dummyInstance1)
    testTree.insert(dummyInstance2)

    val splitTree1 = testTree.split()
    splitTree1.isInstanceOf[Leaf] shouldBe true

    testTree.insert(dummyInstance3)
    testTree.insert(dummyInstance4)
    testTree.insert(dummyInstance5)

    val splitTree2 = testTree.split()
    splitTree2.isInstanceOf[Node] shouldBe true
  }

  "New rules" should "be created" in {
    val minSplit = 3

    val testTree = Leaf(minSplit)

    val dummyInstance1 = new TestType(Dummy(1, 1.0), "A")
    val dummyInstance2 = new TestType(Dummy(2, 1.25), "A")
    val dummyInstance3 = new TestType(Dummy(3, 1.5), "B")
    val dummyInstance4 = new TestType(Dummy(4, 1.75), "B")
    val dummyInstance5 = new TestType(Dummy(5, 2.0), "B")
    val dummyInstance6 = new TestType(Dummy(2, 1.25), "A")

    testTree.insert(dummyInstance1)
    testTree.insert(dummyInstance2)
    testTree.insert(dummyInstance3)
    testTree.insert(dummyInstance4)
    testTree.insert(dummyInstance5)
    testTree.insert(dummyInstance6)
    //TODO figure out how to test
  }

  "CountLeafs" should "give the number of leafs" in {
    val minSplit = 3

    val testTree = Leaf(minSplit)

    val dummyInstance1 = new TestType(Dummy(1, 1.0), "A")
    val dummyInstance2 = new TestType(Dummy(2, 1.25), "A")
    val dummyInstance3 = new TestType(Dummy(3, 1.5), "B")
    val dummyInstance4 = new TestType(Dummy(4, 1.75), "B")
    val dummyInstance5 = new TestType(Dummy(5, 2.0), "B")
    val dummyInstance6 = new TestType(Dummy(2, 1.25), "A")

    testTree.insert(dummyInstance1)
    testTree.insert(dummyInstance2)
    testTree.insert(dummyInstance3)
    testTree.insert(dummyInstance4)
    testTree.insert(dummyInstance5)
    testTree.insert(dummyInstance6)

    testTree.countLeafs() shouldBe 1

    val firstSplit = testTree.split()

    firstSplit.countLeafs() shouldBe 2

    val secondSplit = firstSplit.split()

    secondSplit.countLeafs() shouldBe 2
  }

}