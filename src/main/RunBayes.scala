package main

import bayes.BayesTrainer
import bayes.FeatureTransformer

object RunBayes extends App {
  val transformer = new FeatureTransformer()
  val trainer = BayesTrainer
 
  val training = new TrainingSetRunner()
  training.run(transformer, trainer)
  
  val test = new TestSetRunner()
  test.run(transformer, trainer)
}