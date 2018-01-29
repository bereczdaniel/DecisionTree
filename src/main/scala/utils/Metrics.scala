package utils

object Metrics {

  def accuracy[T](predictions: Array[T], y: Array[T]): Double = {
    predictions
      .zip(y)
      .count(x => x._1 == x._2) / predictions.length.toDouble
  }

  def truePositive(predictions: Array[Int], y: Array[Int]): Int =
    predictions
      .zip(y)
      .count(x => (x._1 == 1) && (x._2 == 1))

  def falsePositive(predictions: Array[Int], y: Array[Int]): Int =
    predictions
      .zip(y)
      .count(x => (x._1 == 1) && (x._2 == 0))

  def precision(predictions: Array[Int], y: Array[Int]): Double = {
    val tp = truePositive(predictions, y)

    val fp = falsePositive(predictions, y)

    tp / (tp+fp).toDouble
  }

  def recall(predictions: Array[Int], y: Array[Int]): Double = {
    val tp = truePositive(predictions, y)

    tp / y.count(x => x ==1).toDouble
  }

  def F1Score(predictions: Array[Int], y: Array[Int]): Double = {
    2 / (1 / recall(predictions, y) + 1 / precision(predictions, y))
  }

}
