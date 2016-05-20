
package bayes

import data.TrainingExample
import main.Classifier
import main.ClassifierTrainer

case class LabelAndFeature(label: String, featureIndex: Int, featureValue: String)

case class BayesClassifier(
    pLabel: Map[String, Double],
    pFeatureGivenLabel: Map[LabelAndFeature, Double]) extends Classifier[String] {

  def classify(features: Vector[String]): String = {
    val probabilities = for {
      label <- pLabel.keySet.toVector
      (feature, i) <- features.zipWithIndex
      p = pFeatureGivenLabel(LabelAndFeature(label, i, feature))
    } yield label -> p

    val pClassification =
      probabilities
        .groupBy(_._1)
        .map {
          case (label, pFeature) => label -> {
            math.log(pLabel(label)) + pFeature.map(_._2).map { math.log }.sum
          }
        }.toVector

    //println(pClassification)
    pClassification.sortBy { _._2 * -1 }.head._1
  }
}

object BayesTrainer extends ClassifierTrainer[String] {
  override def train(examples: Vector[TrainingExample[String]]): BayesClassifier = {
    val totalSize = examples.size.toDouble
    val labelCounts = aggregateByKey(examples.collect { case TrainingExample(_, Some(label)) => label -> 1 })
    val pLabel = labelCounts.map { case (k, v) => k -> v / totalSize }

    val freqs = for {
      TrainingExample(features, Some(label)) <- examples
      (featureValue, featureIndex) <- features.zipWithIndex
      if !featureValue.contains("?")
    } yield LabelAndFeature(label, featureIndex, featureValue) -> 1

    val pFeatureGivenLabel = aggregateByKey(freqs).map {
      case (key, count) => key -> count / labelCounts(key.label).toDouble
    }

    BayesClassifier(pLabel, pFeatureGivenLabel.withDefaultValue(0))
  }

  private def aggregateByKey[K](data: Iterable[(K, Int)]): Map[K, Int] = {
    data
      .groupBy(_._1)
      .map { case (k, v) => k -> v.map(_._2).reduce(_ + _) }
  }
}