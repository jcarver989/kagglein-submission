
package main

import logisticregression.LogisticRegressionTrainer
import logisticregression.RegressionTransformer

object RunOnTest extends App {
  val transformer = new RegressionTransformer()
  val trainer = new LogisticRegressionTrainer(
    learningRate = learningRate,
    regularization = 1.5,
    epochs = 500)

  //val trainingRunner = new TrainingSetRunner()
  //trainingRunner.run(transformer, trainer)

  val testRunner = new TestSetRunner()
  testRunner.run(transformer, trainer)

  def learningRate(epoch: Int): Double = {
    if (epoch < 100) {
      1
    } else if (epoch < 100) {
      0.1
    } else {
      0.05
    }
  }
}