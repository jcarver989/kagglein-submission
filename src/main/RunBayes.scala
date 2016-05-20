package main

import bayes.BayesTrainer
import bayes.FeatureTransformer

object RunBayes extends App {
  import data.Features._
  val transformer = new FeatureTransformer(Set(
    sex,
    fnlwgt,
    //educationNum,
    education,
    nativeCountry,
    maritalStatus,
    //relationship,
    hoursPerWeek
    ))
  
  val trainer = BayesTrainer
 
  val training = new TrainingSetRunner()
  training.run(transformer, trainer)
  
  val test = new TestSetRunner()
  test.run(transformer, trainer)
}