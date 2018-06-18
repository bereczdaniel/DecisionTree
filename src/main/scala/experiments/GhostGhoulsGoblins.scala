package experiments

import instance.{Features, Instance}
import tree.classifier.TreeClassifier
import utils.Metrics
import utils.Utils.trainTestSplit


/**
  * Experiment for the Kaggle competition: https://www.kaggle.com/c/ghouls-goblins-and-ghosts-boo
  */
object GhostGhoulsGoblins {

  case class Data(boneLength: Double, rottingFlesh: Double, hairLength: Double, hasSoul: Double) extends Features {
    override def getValues: Array[Double] = Array(boneLength, rottingFlesh, hairLength, hasSoul)
  }

  case class Record(attributes: Data, label: String) extends Instance(attributes, label)

  def main(args: Array[String]): Unit = {
    val data = scala.io.Source.fromFile("train.csv")
      .getLines()
      .map{ line =>
        val fields = line.split(",")
        Record(Data(fields(1).toDouble, fields(2).toDouble, fields(3).toDouble, fields(4).toDouble), fields(6))
      }
      .toArray

    val (train, eval) = trainTestSplit(data, 0.7, shuffle = false)

    val clf = new TreeClassifier(maxDepth = 3)

    val model = clf.train(train.toArray)


    val predictions = (for (instance <- eval) yield model.predict(instance.attributes)).toArray

    println(Metrics.accuracy(predictions, eval.map(_.label).toArray))
  }

}
