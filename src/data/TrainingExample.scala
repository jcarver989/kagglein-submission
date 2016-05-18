
package data

import scala.io.Source

case class TrainingExample[T](features: Vector[T], label: Option[String] = None)

object TrainingExample {
  def fromFile(filePath: String): Vector[TrainingExample[String]] = {
    val lines = Source.fromFile(filePath).getLines.toVector
    fromData(lines)
  }

  def fromData(data: Iterable[String]): Vector[TrainingExample[String]] = {
    data.map { l =>
      val columns = l.split("\t")
      TrainingExample(label = Some(columns.last), features = columns.dropRight(1).toVector)
    }.toVector
  }
}

object TestSet {
  def fromFile(filePath: String): Vector[Vector[String]] = {
    val lines = Source.fromFile(filePath).getLines.toVector
    lines.map { _.split("\t").toVector }
  }
}