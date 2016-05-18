package data

trait Transformer[T] {
  def transform(trainingExample: Vector[TrainingExample[String]]): Vector[TrainingExample[T]]
  def transformFeatures(examples: Vector[Vector[String]]): Vector[Vector[T]]
}