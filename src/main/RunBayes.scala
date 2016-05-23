package main

import bayes.BayesTrainer
import bayes.FeatureTransformer
import data.TrainingExample
import java.io.PrintWriter
import java.io.File

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
    //hoursPerWeek,
    //capitalLoss,
    //capitalGain
    netGain
  ))

  val trainer = BayesTrainer

  val training = new TrainingSetRunner()
  training.run(transformer, trainer)

  val test = new TestSetRunner()
  test.run(transformer, trainer)
  
  val examples = transformer.transform(TrainingExample.fromFile("training.tsv"))
  val p = new PrintWriter(new File("transformed-data.tsv"))
  examples.foreach { e =>
    p.println(e.features(e.features.size-1))
  }
}