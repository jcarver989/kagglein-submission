package main

import data.TestSet
import data.TrainingExample
import data.Transformer
import java.io.File
import java.io.PrintWriter

class TestSetRunner() {
  def run[T](transformer: Transformer[T], trainer: ClassifierTrainer[T]): Unit = {
    println("Reading File...")
    val examples = transformer.transform(TrainingExample.fromFile("training.tsv"))

    println("Training...")
    val classifier = trainer.train(examples)

    println("Classifying...")
    val test = transformer.transformFeatures(TestSet.fromFile("test.tsv"))
    val guesses = test.map { features =>
      '"' + classifier.classify(features) + '"'
    }
    val json = s"""
{
  "guesses": [
    ${guesses.mkString(",\n")}
   ]
}
  """

    val f = new PrintWriter(new File("guesses.json"))
    f.print(json)
    f.close
    println("Done...")
  }
}