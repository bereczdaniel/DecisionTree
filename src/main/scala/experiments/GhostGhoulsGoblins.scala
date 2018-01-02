package experiments

import instance.{Features, Instance}
import tree.classifier.TreeClassifier

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

    val train = data.take(math.round(data.length * 0.7).toInt)
    val eval = data.takeRight(math.round(data.length * 0.3).toInt)

    val clf = new TreeClassifier(maxDepth = 3)

    val model = clf.train(train)


    val predictions = for (instance <- eval) yield model.predict(instance.attributes)

    val acc = eval.map(_.label)
      .zip(predictions)
      .count(x => x._1 == x._2)

    println(acc.toDouble / predictions.length)
  }

}
