package service

import domain.{ObjectClass, PartitionedData}

class AverageOfNearestNeighborAlgorithm(data: PartitionedData)
  extends AlgorithmTemplate(data) {

  private val averages = data.trainingData.groupBy(_.name).map(grouped => averageObjectClass(grouped._2, grouped._1))

  override def classifyObject(objectClass: ObjectClass): ObjectClass = {
    val distances = averages.map(testingObject => (testingObject, distance(testingObject, objectClass)))
    distances.minBy(_._2)._1
  }

  private def averageObjectClass(data: Iterable[ObjectClass], className: String): ObjectClass = {
    val countOfObjects = data.size
    val characteristics = data.map(_.characteristics)
      .reduce((first, second) => first.map { case (characteristic, value) => (characteristic, value + second(characteristic)) })
      .map { case (characteristic, value) => (characteristic, value / countOfObjects) }
    ObjectClass(className, characteristics)
  }
}
