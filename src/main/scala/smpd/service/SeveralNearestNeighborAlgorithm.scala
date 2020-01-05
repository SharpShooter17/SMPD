package smpd.service

import smpd.domain.{ObjectClass, PartitionedData}

class SeveralNearestNeighborAlgorithm(data: PartitionedData,
                                      countOfNeighborToMatch: Int) extends AlgorithmTemplate(data) {

  override def classifyObject(objectClass: ObjectClass): ObjectClass = {
    val objectDistance = data.trainingData.map(testingObject => ObjectDistance(testingObject.name, distance(testingObject, objectClass)))
      .toSeq
      .groupBy(_.name)
      .map(grouped =>
        ObjectDistance(
          name = grouped._1,
          distance = grouped._2.map(objectDistance => objectDistance.distance).sortBy(value => value).take(countOfNeighborToMatch).sum
        )).minBy(_.distance)

    ObjectClass(objectDistance.name)
  }

  case class ObjectDistance(name: String, distance: Double)

}
