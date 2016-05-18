package logisticregression

import data.TrainingExample
import main.Classifier
import main.ClassifierTrainer

object Math {
  def sigmoid(weights: Vector[Double], features: Vector[Double]): Double = {
    val h = weights.zip(features).map { case (w, f) => w * f }.reduce { _ + _ }
    1 / (1 + math.exp(-h))
  }
}

class LogisticRegressionClassifier(weights: Vector[Double]) extends Classifier[Double] {
  import Math.sigmoid

  override def classify(features: Vector[Double]): String = {
    val guess = sigmoid(weights, 1.0 +: features)
    if (guess > 0.5) ">50K" else "<=50K"
  }
}

class LogisticRegressionTrainer(
    learningRate: (Int) => Double,
    regularization: Double = 1.5,
    epochs: Int = 1000) extends ClassifierTrainer[Double] {
  import Math.sigmoid

  override def train(examples: Vector[TrainingExample[Double]]): LogisticRegressionClassifier = {
    // append 1's to features to make intercept term consistent with rest of features
    val trainingData = examples.map { e => e.copy(features = 1.0 +: e.features) }
    val initialWeights = trainingData.head.features.map { f => math.random }
    var weights = trainingData.head.features.map { f => math.random }
    for (epoch <- 1 to epochs) {
      if (epoch % 10 == 0) {
        println("Starting epoch " + epoch)
        val totalError = examples.map { e =>
          val guess = sigmoid(weights, e.features)
          val answer = transformLabel(e.label.get)
          answer * math.log(guess) + (1 - answer) * math.log(1 - guess)
        }.sum / examples.size.toDouble

        println("Error " + totalError)
      }
      weights = update(trainingData, weights, learningRate(epoch), regularization)
    }

    println(initialWeights)
    println(weights)
    new LogisticRegressionClassifier(weights)
  }

  private def update(
    examples: Vector[TrainingExample[Double]],
    weights: Vector[Double],
    learningRate: Double,
    regularization: Double): Vector[Double] = {
    val errorTerms = examples.map { e => e.features -> (sigmoid(weights, e.features) - transformLabel(e.label.get)) }
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