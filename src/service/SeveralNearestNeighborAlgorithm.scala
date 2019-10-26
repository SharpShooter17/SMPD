package service

import domain.{ObjectClass, PartitionedData}

class SeveralNearestNeighborAlgorithm(dataSet: PartitionedData,
                                      countOfNeighborToMatch: Int)
  extends AlgorithmTemplate(dataSet) {

  override def classifyObject(objectClass: ObjectClass): ObjectClass = {
    dataSet.trainingData.map(testingObject => (testingObject, distance(testingObject, objectClass)))
      .toSeq
      .groupBy(_._1)
      .map(grouped => (grouped._1, grouped._2.map(touple => touple._2).sortBy(value => value).take(countOfNeighborToMatch).sum))
      .minBy(_._2)._1
  }
}
