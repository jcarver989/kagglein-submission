

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

    (featureValues :+ (featureValues(capitalGain).toLong - featureValues(capitalLoss).toLong).toString)
      .zipWithIndex
      .filterNot { case (f, i) => throwAwayFeatures.contains(i) }
      .map {
        case (f, i) if i == education => transformEducation(f)
        case (f, i) if i == age => transformAge(f.toInt)
        case (f, i) if i == nativeCountry => transformCountry(f)
        case (f, i) if i == maritalStatus => transformMarriage(f)
        case (f, i) if i == educationNum => transformEducationNum(f.toInt)
        case (f, i) if i == hoursPerWeek => transformHours(f.toInt)
        case (f, i) if i == relationship => transformRelationship(f)
        case (f, i) if i == netGain => transformNetGain(f.toInt)
        case (f, i) => f
      }
  }

  private def transformRelationship(relationship: String): String = {
    relationship match {
      case "Wife" => "Wife"
      case "Husband" => "Husband"
      case _ => "Unmarried"
    }
  }
  
  private def transformNetGain(gain: Int): String = {
    gain match {
      case g if g < 0 => "loss"
      case g if g == 0 => "none"
      case g if g < 5000 => "5k"
      case g if g < 10000 => "10k"
      case g if g < 20000 => "20k"
      case g if g < 50000 => "50k"
      case _ => "other"
    }
  }

  private def transformEducationNum(num: Int): String = {
    if (num < 10) {
      "0"
    } else if (num < 15) {
      "1"
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

  private def transformAge(age: Int): String = {
    if (age < 18) {
      "1"
    } else if (age < 37) {
      "2"
    } else if (age < 65) {
      "3"
    } else {
      "4"
    }
  }

  private def transformHours(hours: Int): String = {
    if (hours < 6) {
      "1"
    } else if (hours < 15) {
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