package Purity.Measures

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable

class GiniTest extends FlatSpec with PropertyChecks with Matchers{

  "Gini" should "give correct answers" in {
    val classCounter = new mutable.HashMap[String, Int]()

    classCounter += (("A", 5))

    math.abs(Gini.gini(classCounter) - 0.0) should be < 0.0001

    classCounter += (("B", 5))

    math.abs(Gini.gini(classCounter) - 0.5) should be < 0.0001

    classCounter += (("A", 9))
    classCounter += (("B", 1))

    math.abs(Gini.gini(classCounter) - 0.18) should be < 0.0001

    classCounter += (("A", 5))
    classCounter += (("B", 5))
    classCounter += (("C", 5))
    classCounter += (("D", 5))

    math.abs(Gini.gini(classCounter) - 0.75) should be < 0.0001
  }

}
