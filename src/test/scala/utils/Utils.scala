package utils

import Instance.{Features, Instance}

object Utils {

}

case class Dummy(h: Int, w: Double) extends Features{
  override def getValues: Array[Double] = Array[Double](h, w)
}

class TestType(attribute: Dummy, label: String) extends Instance(attribute, label){

}
