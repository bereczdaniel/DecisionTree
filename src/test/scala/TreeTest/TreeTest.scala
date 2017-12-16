package TreeTest

import Tree.{Leaf, Node}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import utils._

class TreeTest extends FlatSpec with PropertyChecks with Matchers {


  "Tree operation" should "work" in {
    val minSplit = 10

    val lf1 = new Leaf[Dummy, TestType](null, null, minSplit)
    val lf2 = new Leaf[Dummy, TestType](null, null, minSplit)
    val lf3 = new Leaf[Dummy, TestType](null, null, minSplit)
    val lf4 = new Leaf[Dummy, TestType](null, null, minSplit)


    val testTree = new Node[Dummy, TestType](
      new Node[Dummy, TestType](
        new Leaf[Dummy, TestType](null, null, minSplit),
        new Leaf[Dummy, TestType](null, null, minSplit),
        {_ => true}
      ),
      new Node[Dummy, TestType](
        new Leaf[Dummy, TestType](null, null, minSplit),
        new Leaf[Dummy, TestType](null, null, minSplit),
        {_ =>  true}
      ),
      {_ => true}
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

}
