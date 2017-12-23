package instance

abstract class Instance(attributes: Features, label: String) {
  def getLabel: String = label
  def getFeatures: Features = attributes
}

abstract class Features(){
  def getValues: Array[Double]
}
