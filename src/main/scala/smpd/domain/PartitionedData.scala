package smpd.domain

class PartitionedData(val trainingData: Iterable[ObjectClass],
                      val testingData: Iterable[ObjectClass])

object PartitionedData {

  def apply(trainingData: Iterable[ObjectClass], testingData: Iterable[ObjectClass]): PartitionedData = {
    new PartitionedData(trainingData, testingData)
  }

  def apply(data: Map[String, Vector[ObjectClass]],
            proportionOfTrainingSet: Double): PartitionedData = {
    val (trainingData, testingData) = dataPartitioning(data, proportionOfTrainingSet)
    new PartitionedData(trainingData, testingData)
  }

  private def dataPartitioning(data: Map[String, Vector[ObjectClass]],
                               proportionOfTrainingSet: Double): (Iterable[ObjectClass], Iterable[ObjectClass]) = {
    val partitionedClasses = data.map {
      case (_, values) => partition(values, proportionOfTrainingSet)
    }.toSeq
    partitionedClasses.reduce((firstPartitionedClass, secondPartitionedClass) =>
      (firstPartitionedClass._1 ++ secondPartitionedClass._1, firstPartitionedClass._2 ++ secondPartitionedClass._2))
  }

  private def partition(data: Iterable[ObjectClass],
                        proportionOfTrainingSet: Double): (Iterable[ObjectClass], Iterable[ObjectClass]) = {
    var i = 0
    val countOfTrainingData = (data.size * proportionOfTrainingSet).toInt
    data.partition(_ => {
      i = i + 1
      i - 1 < countOfTrainingData
    })
  }
}
