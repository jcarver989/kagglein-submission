

package bayes

import data.TrainingExample
import data.Features
import data.Transformer

class FeatureTransformer() extends Transformer[String] {
  import data.Features._

  val throwAwayFeatures = Set(
    sex,
    fnlwgt,
    educationNum,
    nativeCountry,
    relationship)

  override def transform(examples: Vector[TrainingExample[String]]): Vector[TrainingExample[String]] = {
    examples.map { e => e.copy(features = transformFeatureVector(e.features)) }
  }

  override def transformFeatures(examples: Vector[Vector[String]]) = {
    examples.map { transformFeatureVector }
  }

  private def transformFeatureVector(featureValues: Vector[String]): Vector[String] = {
    featureValues.zipWithIndex.collect {
      case (f, i) if i == education => transformEducation(f)
      case (f, i) if i == age => transformAge(f.toInt)
      case (f, i) if i == hoursPerWeek => transformHours(f.toInt)
      case (f, i) if i == capitalGain => transformCapitalGain(f.toInt)
      case (f, i) if !throwAwayFeatures.contains(i) => f
    }
  }

  private def transformCapitalGain(gain: Int): String = {
    if (gain < 7000) {
      "low"
    } else if (gain < 15000) {
      "medium"
    } else {
      "high"
    }
  }

  private def transformAge(age: Int): String = {
    if (age < 18) {
      "minor"
    } else if (age > 65) {
      "retired"
    } else {
      "normal"
    }
  }

  private def transformHours(hours: Int): String = {
    if (age < 40) {
      "part-time"
    } else if (age < 60) {
      "full-time"
    } else {
      "over-time"
    }
  }

  private def transformEducation(education: String): String = {
    education match {
      case "Bachelors" => "Undergrad"
      case "Masters" | "Doctorate" => "Higher"
      case _ => "HS-grad"
    }
  }
}