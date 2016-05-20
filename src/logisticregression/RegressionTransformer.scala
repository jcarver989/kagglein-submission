

package logisticregression

import data.Features
import data.TrainingExample
import data.Transformer
import data.Util
import bayes.FeatureTransformer

class RegressionTransformer(featureTransformer: FeatureTransformer, featuresToIgnore: Set[Int]) extends Transformer[Double] {
  import data.Features._

  /* val featuresToInclude = Set[Int](
    age,
    employment,
    //fnlwgt,
    education,
    //educationNum,
    //maritalStatus,
    occupation,
    //relationship,
    //race,
    sex,
    capitalGain,
    capitalLoss,
    hoursPerWeek
    //nativeCountry
  ) */

  override def transform(data: Vector[TrainingExample[String]]): Vector[TrainingExample[Double]] = {
    val examples = featureTransformer.transform(data)
    val featureMap =
      {
        val map = scala.collection.mutable.Map[Int, Set[String]]().withDefaultValue(Set.empty)

        for {
          example <- examples
          (feature, index) <- example.features.zipWithIndex
        } {
          map(index) = (map(index) + feature)
        }
        map.toMap
      }

    val transformed = examples.map { e =>
      val regressionFeatures = transformFeatureVector(e.features, featureMap)
      TrainingExample(features = regressionFeatures, label = e.label)
    }

    Util.normalizeTraining(transformed)
  }

  private def transformFeatureVector(featureValues: Vector[String], featureMap: Map[Int, Set[String]]): Vector[Double] = {
    val netGain = featureValues(capitalGain).toDouble - featureValues(capitalLoss).toDouble
    val features = featureValues.zipWithIndex.collect {
      case (featureValue, index) if !featuresToIgnore.contains(index) => featureTypes(index) match {
        case NumericFeature => Vector(featureValue.toDouble)
        case CategoricalFeature => CategoricalFeature.dummyEncode(featureValue, featureMap(index))
      }
    }.flatten

    features //:+ netGain
  }
}

