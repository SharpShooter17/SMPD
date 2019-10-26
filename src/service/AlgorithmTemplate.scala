package service

import domain.{ObjectClass, PartitionedData}

import scala.math.pow

abstract class AlgorithmTemplate(data: PartitionedData) {

  def distance(first: ObjectClass, second: ObjectClass): Double =
    first.characteristics.map {
      case (characteristic, value) =>
        val sumOfCharacteristics = value - second.characteristics(characteristic)
        pow(sumOfCharacteristics, 2)
    }.sum


  def algorithm(): Unit = {
    data.testingData.foreach(testingObject => {
      val classification = classifyObject(testingObject)
      println(s"Matched: ${classification.name == testingObject.name} " +
        s"Original classification: ${testingObject.name} - Algorithm classification: ${classification.name}")
    })
  }

  def classifyObject(objectClass: ObjectClass): ObjectClass

}
