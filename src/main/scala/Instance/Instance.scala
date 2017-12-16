package Instance

abstract class Instance[X](attributes: X, label: String) {
  def getLabel: String = label
  def getAttributes: X = attributes
}
