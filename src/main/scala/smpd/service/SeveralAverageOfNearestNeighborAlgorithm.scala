package smpd.service

import smpd.domain.{ObjectClass, PartitionedData}


// TODO: Fixme me please!
class SeveralAverageOfNearestNeighborAlgorithm(partitionedData: PartitionedData)
  extends AlgorithmTemplate(partitionedData) {

  private var allData = partitionedData.testingData ++ partitionedData.trainingData
  private var averages = allData.groupBy(_.name).map(grouped => averageObjectClass(grouped._2.take(2), grouped._1))

  override def algorithm(): Unit = {
    countClassNumbers
    var changedClass = false
    var countOfIterations = 0
    do {
      changedClass = false
      allData = allData.map(data => {
        val classification = classifyObject(data)
        if (data.name != classification.name) {
          changedClass = true
          data.copy(name = classification.name)
        } else {
          data
        }
      })
      countOfIterations = countOfIterations + 1
    } while (changedClass)
    println("After algorithm--------------------------------------------------------------------------------------------")
    println(s"Count of iterations $countOfIterations")
    countClassNumbers
  }

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

  private def countClassNumbers: Unit = {
    allData.groupBy(_.name)
      .map(grouped => (grouped._1, grouped._2.size))
      .foreach(countOfClassObject => println(s"${countOfClassObject._1}: ${countOfClassObject._2}"))
  }
}
