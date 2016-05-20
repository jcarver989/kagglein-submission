package data

case class MinMax(min: Double, max: Double) {
  def +(other: MinMax): MinMax = {
    MinMax(
      if (other.min < min) other.min else min,
      if (other.max > max) other.max else max)
  }
}

object MinMax {
  def apply(n: Double): MinMax = {
    MinMax(n, n)
  }
}

object Util {
  import Features._

  def minMaxByFeature(data: Vector[TrainingExample[String]]): Map[Int, MinMax] = {
    val map = scala.collection.mutable.Map[Int, MinMax]()
    for {
      example <- data
      (feature, index) <- example.features.zipWithIndex
      if featureTypes(index) == NumericFeature
    } {
      val n = feature.toDouble
      map(index) = (MinMax(n) + map.getOrElse(index, MinMax(n)))
    }
    map.toMap
  }

  def normalizeTraining(data: Vector[TrainingExample[Double]]): Vector[TrainingExample[Double]] = {
    val minMaxMap = {
      val map = scala.collection.mutable.Map[Int, MinMax]()
      for {
        example <- data
        (feature, index) <- example.features.zipWithIndex
      } {
        map(index) = (MinMax(feature) + map.getOrElse(index, MinMax(feature)))
      }
      map.toMap
    }

    data.map { example => example.copy(features = normalize(minMaxMap, example.features)) }
  }

  def normalize(minMaxMap: Map[Int, MinMax], featureValues: Vector[Double]): Vector[Double] = {
    featureValues.zipWithIndex.map {
      case (f, i) if minMaxMap(i) == MinMax(0, 1) => f
      case (f, i) =>
        val minMax = minMaxMap(i)
        (f - minMax.min + 1) / (minMax.max - minMax.min + 1)
    }
  }
}