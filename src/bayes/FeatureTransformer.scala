

package bayes

import data.TrainingExample
import data.Features
import data.Transformer

class FeatureTransformer() extends Transformer[String] {
  import data.Features._

  val throwAwayFeatures = Set(
    sex,
    fnlwgt,
    nativeCountry,
    hoursPerWeek)

  override def transform(examples: Vector[TrainingExample[String]]): Vector[TrainingExample[String]] = {
    examples.map { e => e.copy(features = transformFeatureVector(e.features)) }
  }

  override def transformFeatures(examples: Vector[Vector[String]]) = {
    examples.map { transformFeatureVector }
  }

  private def transformFeatureVector(featureValues: Vector[String]): Vector[String] = {
    featureValues.zipWithIndex.collect {
      case (f, i) if i == education => transformEducation(f)
      case (f, i) if !throwAwayFeatures.contains(i) => f
    }
  }

  private def transformEducation(education: String): String = {
    education match {
      case "Bachelors" | "Masters" | "Doctorate" => "Bachelors"
      case _ => "HS-grad"
    }
  }
}