package data

import scala.Vector

object Features {
  sealed trait Feature {
    def isNumeric(): Boolean
  }

  case class CategoricalFeature(values: Map[String, String]) extends Feature {
    val valuesVector = values.values.toVector

    override def isNumeric(): Boolean = false

    def dummyEncode(value: String): Vector[Double] = {
      val mappedValue = values(value)
      val index = valuesVector.indexOf(mappedValue)
      valuesVector.zipWithIndex.map {
        case (v, i) if i == index => 1.0
        case _ => 0.0
      }
    }
  }

  case object NumericFeature extends Feature {
    override def isNumeric(): Boolean = true
  }

  val employmentValues = CategoricalFeature(Map(
    "Private" -> "non gov",
    "Self-emp-not-inc" -> "self",
    "Self-emp-inc" -> "self",
    "Federal-gov" -> "fed-gov",
    "Local-gov" -> "gov",
    "State-gov" -> "gov",
    "Without-pay" -> "unemp",
    "Never-worked" -> "enemp",
    "?" -> "non gov"
  ))

  val educationValues = CategoricalFeature(Map(
    "Bachelors" -> "undergrad",
    "Some-college" -> "some college",
    "11th" -> "no college",
    "HS-grad" -> "no college",
    "Prof-school" -> "some college",
    "Assoc-acdm" -> "some college",
    "Assoc-voc" -> "some college",
    "9th" -> "no college",
    "7th-8th" -> "no college",
    "12th" -> "no college",
    "Masters" -> "masters",
    "1st-4th" -> "no college",
    "10th" -> "no college",
    "Doctorate" -> "doctorate",
    "5th-6th" -> "no college",
    "Preschool" -> "no college",
    "?" -> "no college"))

  val maritalStatusValues = CategoricalFeature(Map(
    "Married-civ-spouse" -> "married",
    "Divorced" -> "not married",
    "Never-married" -> "not married",
    "Separated" -> "not married",
    "Widowed" -> "not married",
    "Married-spouse-absent" -> "not married",
    "Married-AF-spouse" -> "married",
    "?" -> "not marrried"
  ))

  val occupationValues = CategoricalFeature(Map(
    "Tech-support" -> "bluecollar",
    "Craft-repair" -> "bluecollar",
    "Other-service" -> "other",
    "Sales" -> "whitecollar",
    "Exec-managerial" -> "whitecollar",
    "Prof-specialty" -> "specialty",
    "Handlers-cleaners" -> "service",
    "Machine-op-inspct" -> "bluecollar",
    "Adm-clerical" -> "whitecollar",
    "Farming-fishing" -> "farming",
    "Transport-moving" -> "bluecollar",
    "Priv-house-serv" -> "service",
    "Protective-serv" -> "service",
    "Armed-Forces" -> "army",
    "?" -> "bluecollar"
  ))

  val relationshipValues = CategoricalFeature(Map(
    "Wife" -> "wife",
    "Own-child" -> "child",
    "Husband" -> "husband",
    "Not-in-family" -> "other",
    "Other-relative" -> "other",
    "Unmarried" -> "other",
    "?" -> "other"
  ))

  val raceValues = CategoricalFeature(Map(
    "White" -> "white",
    "Asian-Pac-Islander" -> "asian",
    "Amer-Indian-Eskimo" -> "indian",
    "Other" -> "other",
    "Black" -> "black",
    "?" -> "other"
  ))

  val sexValues = CategoricalFeature(Map(
    "Male" -> "male",
    "Female" -> "female",
    "?" -> "other"
  ))

  val nativeCountryValues = CategoricalFeature(Map(
    "United-States" -> "us",
    "Cambodia" -> "asia",
    "England" -> "europe",
    "Puerto-Rico" -> "central-america",
    "Canada" -> "north-america",
    "Germany" -> "europe",
    "Outlying-US(Guam-USVI-etc)" -> "other",
    "India" -> "india",
    "Japan" -> "asia",
    "Greece" -> "europe",
    "South" -> "other",
    "China" -> "asia",
    "Cuba" -> "central-america",
    "Iran" -> "middle-east",
    "Honduras" -> "central-america",
    "Philippines" -> "asia",
    "Italy" -> "europe",
    "Poland" -> "europe",
    "Jamaica" -> "other",
    "Vietnam" -> "asia",
    "Mexico" -> "central-america",
    "Portugal" -> "europe",
    "Ireland" -> "europe",
    "France" -> "europe",
    "Dominican-Republic" -> "central-america",
    "Laos" -> "asia",
    "Ecuador" -> "central-america",
    "Taiwan" -> "asia",
    "Haiti" -> "central-america",
    "Columbia" -> "central-america",
    "Hungary" -> "europe",
    "Guatemala" -> "central-america",
    "Nicaragua" -> "central-america",
    "Scotland" -> "europe",
    "Thailand" -> "asia",
    "Yugoslavia" -> "europe",
    "El-Salvador" -> "central-america",
    "Trinadad&Tobago" -> "europe",
    "Peru" -> "central-america",
    "Hong" -> "asia",
    "Holand-Netherlands" -> "europe",
    "?" -> "other"
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