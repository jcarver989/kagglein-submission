

package logisticregression

import data.TrainingExample
import data.Features
import data.Features.features
import data.Transformer

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

class RegressionTransformer extends Transformer[Double] {
  import data.Features._

  val throwAwayFeatures = Set[Int](fnlwgt)

  override def transformFeatures(examples: Vector[Vector[String]]): Vector[Vector[Double]] = {
    normalizeFeatures(examples.map { transformFeatureVector })
  }

  override def transform(examples: Vector[TrainingExample[String]]): Vector[TrainingExample[Double]] = {
    val transformed = examples.map { e =>
      val regressionFeatures = transformFeatureVector(e.features)
      TrainingExample(features = regressionFeatures, label = e.label)
    }

    normalizeTraining(transformed)
  }

  private def normalizeFeatures(data: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    val minMaxMap = {
      val map = scala.collection.mutable.Map[Int, MinMax]()
      for {
        example <- data
        (feature, index) <- example.zipWithIndex
      } {
        map(index) = (MinMax(feature) + map.getOrElse(index, MinMax(feature)))
      }
      map.toMap
    }

    data.map { features => normalize(minMaxMap, features) }
  }

  private def normalizeTraining(data: Vector[TrainingExample[Double]]): Vector[TrainingExample[Double]] = {
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

  private def normalize(minMaxMap: Map[Int, MinMax], featureValues: Vector[Double]): Vector[Double] = {
    featureValues.zipWithIndex.map {
      case (f, i) =>
        val minMax = minMaxMap(i)
        (f - minMax.min) / (minMax.max - minMax.min)
    }
  }

  private def transformFeatureVector(featureValues: Vector[String]): Vector[Double] = {
    featureValues.zipWithIndex.collect {
      case (featureValue, index) if !throwAwayFeatures.contains(index) => features(index) match {
        case NumericFeature => Vector(featureValue.toDouble)
        case c: CategoricalFeature => c.dummyEncode(featureValue)
      }
    }.flatten
  }
}

