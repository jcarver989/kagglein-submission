package data

import scala.Vector

object Features {
  sealed trait Feature
  case class CategoricalFeature(possibleValues: Set[String]) extends Feature {
    val valuesVector = possibleValues.toVector
    val map = possibleValues.toVector.zipWithIndex.map { case (value, index) => value -> (index + 1.0) }.toMap

    def dummyEncode(value: String): Vector[Double] = {
      val index = valuesVector.indexOf(value)
      valuesVector.zipWithIndex.map {
        case (v, i) if i == index => 1.0
        case _ => 0.0
      }
    }
  }

  case object NumericFeature extends Feature

  val employmentValues = CategoricalFeature(Set(
    "Private",
    "Self-emp-not-inc",
    "Self-emp-inc",
    "Federal-gov",
    "Local-gov",
    "State-gov",
    "Without-pay",
    "Never-worked",
    "?"
  ))

  val educationValues = CategoricalFeature(Set(
    "Bachelors",
    "Some-college",
    "11th",
    "HS-grad",
    "Prof-school",
    "Assoc-acdm",
    "Assoc-voc",
    "9th",
    "7th-8th",
    "12th",
    "Masters",
    "1st-4th",
    "10th",
    "Doctorate",
    "5th-6th",
    "Preschool",
    "?"))

  val maritalStatusValues = CategoricalFeature(Set(
    "Married-civ-spouse",
    "Divorced",
    "Never-married",
    "Separated",
    "Widowed",
    "Married-spouse-absent",
    "Married-AF-spouse",
    "?"
  ))

  val occupationValues = CategoricalFeature(Set(
    "Tech-support",
    "Craft-repair",
    "Other-service",
    "Sales",
    "Exec-managerial",
    "Prof-specialty",
    "Handlers-cleaners",
    "Machine-op-inspct",
    "Adm-clerical",
    "Farming-fishing",
    "Transport-moving",
    "Priv-house-serv",
    "Protective-serv",
    "Armed-Forces",
    "?"
  ))

  val relationshipValues = CategoricalFeature(Set(
    "Wife",
    "Own-child",
    "Husband",
    "Not-in-family",
    "Other-relative",
    "Unmarried",
    "?"
  ))

  val raceValues = CategoricalFeature(Set(
    "White",
    "Asian-Pac-Islander",
    "Amer-Indian-Eskimo",
    "Other",
    "Black",
    "?"
  ))

  val sexValues = CategoricalFeature(Set(
    "Male",
    "Female",
    "?"
  ))

  val nativeCountryValues = CategoricalFeature(Set(
    "United-States",
    "Cambodia",
    "England",
    "Puerto-Rico",
    "Canada",
    "Germany",
    "Outlying-US(Guam-USVI-etc)",
    "India",
    "Japan",
    "Greece",
    "South",
    "China",
    "Cuba",
    "Iran",
    "Honduras",
    "Philippines",
    "Italy",
    "Poland",
    "Jamaica",
    "Vietnam",
    "Mexico",
    "Portugal",
    "Ireland",
    "France",
    "Dominican-Republic",
    "Laos",
    "Ecuador",
    "Taiwan",
    "Haiti",
    "Columbia",
    "Hungary",
    "Guatemala",
    "Nicaragua",
    "Scotland",
    "Thailand",
    "Yugoslavia",
    "El-Salvador",
    "Trinadad&Tobago",
    "Peru",
    "Hong",
    "Holand-Netherlands",
    "?"
  ))

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

  val features = Vector(
    NumericFeature,
    employmentValues,
    NumericFeature,
    educationValues,
    NumericFeature,
    maritalStatusValues,
    occupationValues,
    relationshipValues,
    raceValues,
    sexValues,
    NumericFeature,
    NumericFeature,
    NumericFeature,
    nativeCountryValues
  )
}