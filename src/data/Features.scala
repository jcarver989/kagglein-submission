package data

import scala.Vector

object Features {
  sealed trait Feature {
    def isNumeric(): Boolean
  }

  case object CategoricalFeature extends Feature {
    override def isNumeric(): Boolean = false

    def dummyEncode(value: String, values: Set[String]): Vector[Double] = {
      val valuesVector = values.toVector
      val index = valuesVector.indexOf(value)
      valuesVector.zipWithIndex.map {
        case (v, i) if i == index => 1.0
        case _ => 0.0
      }
    }
  }

  case object NumericFeature extends Feature {
    override def isNumeric(): Boolean = true
  }

  val age = 0
  val employment = 1
  val fnlwgt = 2
  val education = 3
  val educationNum = 4
  val maritalStatus = 5
  val occupation = 6
  val relationship = 7
  val race = 8
  val sex = 9
  val capitalGain = 10
  val capitalLoss = 11
  val hoursPerWeek = 12
  val nativeCountry = 13

  val featureTypes = Map(
    age -> NumericFeature,
    employment -> CategoricalFeature,
    fnlwgt -> NumericFeature,
    education -> CategoricalFeature,
    educationNum -> CategoricalFeature,
    maritalStatus -> CategoricalFeature,
    occupation -> CategoricalFeature,
    relationship -> CategoricalFeature,
    race -> CategoricalFeature,
    sex -> CategoricalFeature,
    capitalGain -> NumericFeature,
    capitalLoss -> NumericFeature,
    hoursPerWeek -> NumericFeature,
    nativeCountry -> CategoricalFeature
  )

}