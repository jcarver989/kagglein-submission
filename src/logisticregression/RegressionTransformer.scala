

package logisticregression

import data.Features
import data.Features.features
import data.TrainingExample
import data.Transformer
import data.Util


class RegressionTransformer extends Transformer[Double] {
  import data.Features._

  val featuresToInclude = Set[Int](
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
  )

  override def transformFeatures(examples: Vector[Vector[String]]): Vector[Vector[Double]] = {
    Util.normalizeFeatures(examples.map { transformFeatureVector })
  }

  override def transform(examples: Vector[TrainingExample[String]]): Vector[TrainingExample[Double]] = {
    val transformed = examples.map { e =>
      val regressionFeatures = transformFeatureVector(e.features)
      TrainingExample(features = regressionFeatures, label = e.label)
    }

    Util.normalizeTraining(transformed)
  }

  private def transformFeatureVector(featureValues: Vector[String]): Vector[Double] = {
    featureValues.zipWithIndex.collect {
      case (featureValue, index) if featuresToInclude.contains(index) => features(index) match {
        case NumericFeature => Vector(featureValue.toDouble)
        case c: CategoricalFeature => c.dummyEncode(featureValue)
      }
    }.flatten
  }
}

