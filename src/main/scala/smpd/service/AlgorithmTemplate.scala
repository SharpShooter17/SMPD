package smpd.service

import smpd.domain.{ObjectClass, PartitionedData}

import scala.math.{pow, sqrt}

abstract class AlgorithmTemplate(partitionedData: PartitionedData) {

  protected def distance(first: ObjectClass, second: ObjectClass): Double =
    sqrt(
      first.characteristics.map {
        case (characteristic, value) =>
          val sumOfCharacteristics = value - second.characteristics(characteristic)
          pow(sumOfCharacteristics, 2)
      }.sum
    )

  def algorithm(): Unit = {
    val results = partitionedData.testingData.map(testingObject => Matching(testingObject, classifyObject(testingObject)))
    val matched = results.count(result => result.testedObject.name == result.classificationObject.name)
    val percentOfMatchedObjects = (matched.doubleValue() / partitionedData.testingData.size.doubleValue()) * 100
    println(s"All testing object: ${partitionedData.testingData.size}, Matched objects: $matched, Percent: $percentOfMatchedObjects%")
  }

  protected def classifyObject(objectClass: ObjectClass): ObjectClass

  case class Matching(testedObject: ObjectClass, classificationObject: ObjectClass)

}
