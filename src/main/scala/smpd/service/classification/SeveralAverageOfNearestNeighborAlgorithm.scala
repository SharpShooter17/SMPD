package smpd.service.classification

import smpd.domain.{ObjectClass, PartitionedData}

class SeveralAverageOfNearestNeighborAlgorithm(partitionedData: PartitionedData)
  extends AlgorithmTemplate(partitionedData) {

  private val averages = partitionedData.trainingData.groupBy(_.name).map(grouped => averageObjectClass(grouped._2.take(2), grouped._1))

  override def classifyObject(objectClass: ObjectClass): ObjectClass = {
    val distances = averages.map(testingObject => (testingObject, distance(testingObject, objectClass)))
    distances.minBy(_._2)._1
  }

  private def averageObjectClass(data: Iterable[ObjectClass], className: String): ObjectClass = {
    val countOfObjects = data.size
    val characteristics: Map[String, Double] = data.map(_.characteristics)
      .reduce((first, second) => first.map { case (characteristic, value) => (characteristic, value + second(characteristic)) })
      .map { case (characteristic, value) => (characteristic, value / countOfObjects) }
    ObjectClass(className, characteristics)
  }
}
