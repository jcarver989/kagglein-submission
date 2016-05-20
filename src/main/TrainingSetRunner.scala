package main

import data.Features
import data.TrainingExample
import data.Transformer

class TrainingSetRunner() {
  import data.Features._

  def run[T](transformer: Transformer[T], trainer: ClassifierTrainer[T]): Unit = {
    println("Reading File...")
    val examples = transformer.transform(TrainingExample.fromFile("training.tsv"))
    val cutoff = (examples.size * 0.8).toInt

    println("Partitioning Test/Training set...")
    val (training, test) = examples.zipWithIndex.partition { case (e, i) => i < cutoff }

    println("Training...")
    val classifier = trainer.train(training.map { _._1 })

    println("Classifying...")
    val (correct, incorrect) = test
      .map {
        case (e, _) =>
          val guess = classifier.classify(e.features)
          guess == e.label.get
      }
      .partition { wasCorrect => wasCorrect }

    println(correct.size / (correct.size + incorrect.size).toDouble)
  }
}
  