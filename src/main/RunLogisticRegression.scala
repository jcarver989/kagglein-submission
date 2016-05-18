
package main

import logisticregression.LogisticRegressionTrainer
import logisticregression.RegressionTransformer

object RunOnTest extends App {
  val transformer = new RegressionTransformer()
  val trainer = new LogisticRegressionTrainer(
    learningRate = 0.1,
    regularization = 1.5,
    epochs = 100)

  val trainingRunner = new TrainingSetRunner()
  trainingRunner.run(transformer, trainer)

  //val testRunner = new TestSetRunner()
  //testRunner.run(transformer, trainer)
}