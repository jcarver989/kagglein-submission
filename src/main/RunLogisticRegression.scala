
package main

import logisticregression.LogisticRegressionTrainer
import logisticregression.RegressionTransformer
import bayes.FeatureTransformer
import data.Features._

object RunOnTest extends App {
  val transformer = new RegressionTransformer(
    new FeatureTransformer(Set.empty),
    Set(
      //sex,
      fnlwgt,
      educationNum,
      //nativeCountry,
      relationship
    ))

  val trainer = new LogisticRegressionTrainer(
    learningRate = learningRate,
    regularization = 0.1,
    epochs = 1000)

  //val trainingRunner = new TrainingSetRunner()
  //trainingRunner.run(transformer, trainer)

  val testRunner = new TestSetRunner()
  testRunner.run(transformer, trainer)

  def learningRate(epoch: Int): Double = {
    return 1
    if (epoch < 1000) {
      2.0
    } else if (epoch < 4500) {
      1.0
    } else {
      0.1
    }
  }
}