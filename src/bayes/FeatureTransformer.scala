

package bayes

import data.TrainingExample
import data.Features
import data.Transformer

class FeatureTransformer(throwAwayFeatures: Set[Int]) extends Transformer[String] {
  import data.Features._

  override def transform(examples: Vector[TrainingExample[String]]): Vector[TrainingExample[String]] = {
    examples.map { e => e.copy(features = transformFeatureVector(e.features)) }
  }

  private def transformFeatureVector(featureValues: Vector[String]): Vector[String] = {
    featureValues.zipWithIndex
      .filterNot { case (f, i) => throwAwayFeatures.contains(i) }
      .map {
        case (f, i) if i == education => transformEducation(f)
        case (f, i) if i == age => transformAge(f.toInt)
        case (f, i) if i == nativeCountry => transformCountry(f)
        case (f, i) if i == maritalStatus => transformMarriage(f)
        case (f, i) if i == educationNum => transformEducationNum(f.toInt)
        //case (f, i) if i == hoursPerWeek => transformHours(f.toInt)
        //case (f, i) if i == capitalGain => transformCapitalGain(f.toInt)
        case (f, i) => f
      }
  }
  
  private def transformEducationNum(num: Int): String = {
    if (num < 5) {
      "0"
    } else if (num < 10) {
      "1"
    } else if (num < 15) {
      "2"
    } else {
      "3"
    }
  }

  private def transformMarriage(marriage: String): String = {
    marriage match {
      case "Married-civ-spouse" | "Married-spouse-absent" | "Married-AF-spouse" => "1"
      case "Divorced" | "Never-married" | "Separated" | "Widowed" => "0"
    }
  }

  private def transformCapitalGain(gain: Int): String = {
    if (gain < 0) {
      "1"
    } else if (gain < 10000) {
      "2"
    } else {
      "3"
    }
  }

  private def transformAge(age: Int): String = {
    if (age < 18) {
      "1"
    } else if (age > 65) {
      "2"
    } else {
      "3"
    }
  }

  private def transformHours(hours: Int): String = {
    if (hours < 40) {
      "1"
    } else if (hours < 60) {
      "2"
    } else {
      "3"
    }
  }

  private def transformCountry(country: String): String = {
    country match {
      case "United-States" | "England" => "1"
      //case "England" | "Portugal" | "France" | "Ireland" | "Scotland" => "2"
      case _ => "0"
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