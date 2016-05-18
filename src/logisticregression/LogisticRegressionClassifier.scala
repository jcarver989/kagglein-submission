package logisticregression

import data.TrainingExample
import main.Classifier
import main.ClassifierTrainer

object Util {
  def sigmoid(weights: Vector[Double], features: Vector[Double]): Double = {
    val h = weights.zip(features).map { case (w, f) => w * f }.reduce { _ + _ }
    1 / (1 + math.exp(-h))
  }
}

class LogisticRegressionClassifier(weights: Vector[Double]) extends Classifier[Double] {
  import Util.sigmoid

  override def classify(features: Vector[Double]): String = {
    val guess = sigmoid(weights, 1.0 +: features)
    if (guess > 0.5) ">50K" else "<=50K"
  }
}

class LogisticRegressionTrainer(
    learningRate: Double = 0.1,
    regularization: Double = 1.5,
    epochs: Int = 1000) extends ClassifierTrainer[Double] {
  import Util.sigmoid

  override def train(examples: Vector[TrainingExample[Double]]): LogisticRegressionClassifier = {
    // append 1's to features to make intercept term consistent with rest of features
    val trainingData = examples.map { e => e.copy(features = 1.0 +: e.features) }
    val initialWeights = examples.head.features.map { f => math.random }

    val weights = (1 to epochs).foldLeft(initialWeights) {
      case (weights, epoch) =>
        if (epoch % 10 == 0) println("Starting epoch " + epoch)
        update(trainingData, weights, learningRate, regularization)
    }

    new LogisticRegressionClassifier(weights)
  }

  private def update(
    examples: Vector[TrainingExample[Double]],
    weights: Vector[Double],
    learningRate: Double,
    regularization: Double): Vector[Double] = {
    val errorTerms = examples.map { e => e.features -> (sigmoid(weights, e.features) - transformLabel(e.label)) }
    val regularizationTerm = (1 - learningRate * (regularization / examples.size.toDouble))
    weights.zipWithIndex.map {
      case (w, i) =>
        // do not regularize the intercept "feature"
        val reg = if (i == 0) 1 else regularizationTerm
        val errorSum = errorTerms.map { case (features, error) => error * features(i) }.sum
        w * reg - learningRate * (errorSum / examples.size.toDouble)
    }
  }

  private def transformLabel(label: String): Double = {
    if (label == "<=50K") 0 else 1
  }
}