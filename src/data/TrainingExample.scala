
package data

import scala.io.Source

case class TrainingExample[T](features: Vector[T], label: Option[String] = None)

object TrainingExample {
  def fromFile(filePath: String, hasTarget: Boolean = true): Vector[TrainingExample[String]] = {
    val lines = Source.fromFile(filePath).getLines.toVector
    fromData(lines, hasTarget)
  }

  def fromData(data: Iterable[String], hasTarget: Boolean = true): Vector[TrainingExample[String]] = {
    data.map { l =>
      val columns = l.split("\t")
      if (hasTarget) {
        TrainingExample(label = Some(columns.last), features = columns.dropRight(1).toVector)
      } else {
        TrainingExample(label = None, features = columns.toVector)
      }
    }.toVector
  }
}