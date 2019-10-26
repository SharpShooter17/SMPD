package service

import domain.{ObjectClass, PartitionedData}

class NearestNeighborAlgorithm(data: PartitionedData)
  extends AlgorithmTemplate(data) {

  override def classifyObject(objectClass: ObjectClass): ObjectClass = {
    val distances = data.trainingData.map(testingObject => (testingObject, distance(testingObject, objectClass)))
    distances.minBy(_._2)._1
  }
}
