package main

import data.TrainingExample

trait Classifier[T] {
  def classify(features: Vector[T]): String
}

trait ClassifierTrainer[T] {
  def train(examples: Vector[TrainingExample[T]]): Classifier[T]
}