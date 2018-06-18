package instance

/**
  * Represents a labeled training point
  * @param attributes: Features
  * @param label: Type information for the classification
  */
abstract class Instance(attributes: Features, label: String) {
  def getLabel: String = label
  def getFeatures: Features = attributes
}


/**
  * Represents the set of features. Don't support categorical values.
  */
abstract class Features(){
  def getValues: Array[Double]
}
