package smpd

import smpd.domain.{Data, ObjectClass, PartitionedData}
import smpd.service.FisherExtractionService.filterCharacteristics
import smpd.service.{AverageOfNearestNeighborAlgorithm, FisherExtractionService, NearestNeighborAlgorithm, SeveralNearestNeighborAlgorithm}

import scala.io.Source._

object SmpdApp extends App {

  val rawData = loadDataFromFile(filename = "data.txt")

  run()

  private def loadDataFromFile(filename: String): Data = {
    val source = fromFile(s"resources/$filename")
    val lines = source.getLines()

    val result = lines.map(line => {
      val elements = line.split(",")
      var i = 0
      ObjectClass(
        name = elements.head,
        characteristics = elements.tail.map(value => {
          i = i + 1
          (s"C${i - 1}", value.toDouble)
        }).toMap
      )
    }).toVector.groupBy(_.name)

    source.close()
    result
  }

  private def run(): Unit = {
    val dataWithExtractedCharacteristics = FisherExtractionService.extraction(rawData, 3)
    val data = PartitionedData(data = dataWithExtractedCharacteristics, proportionOfTrainingSet = 0.25)

    println("NearestNeighborAlgorithm-------------------------------------------------------------------------------------")
    new NearestNeighborAlgorithm(data).algorithm()

    println("SeveralNearestNeighborAlgorithm-k=1--------------------------------------------------------------------------")
    new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 1).algorithm()

    println("SeveralNearestNeighborAlgorithm-k=2--------------------------------------------------------------------------")
    new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 2).algorithm()

    println("SeveralNearestNeighborAlgorithm-k=3--------------------------------------------------------------------------")
    new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 3).algorithm()

    println("SeveralNearestNeighborAlgorithm-k=5--------------------------------------------------------------------------")
    new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 6).algorithm()

    println("SeveralNearestNeighborAlgorithm-k=10-------------------------------------------------------------------------")
    new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 10).algorithm()

    println("AverageOfNearestNeighborAlgorithm----------------------------------------------------------------------------")
    new AverageOfNearestNeighborAlgorithm(data = data).algorithm()

    //  println("AverageOfNearestNeighborAlgorithm----------------------------------------------------------------------------")
    //  new SeveralAverageOfNearestNeighborAlgorithm(partitionedData = data).algorithm()
  }

  private def bruteForce(data: Data): Unit = {
    val characteristics = data.headOption.toList.flatMap(_._2).headOption.map(_.characteristics).toList.flatMap(_.keys)
    val characteristicsCombinations = characteristics.combinations(3).toList

    val theBest = characteristicsCombinations.map(combination => {
      val filteredData = data.map {
        case (className, samples) =>
          val mappedSamples = samples.map(objectClass => objectClass.copy(characteristics = filterCharacteristics(combination, objectClass.characteristics)))
          (className, mappedSamples)
      }
      val partitionedData = PartitionedData(data = filteredData, proportionOfTrainingSet = 0.25)
      (combination, new AverageOfNearestNeighborAlgorithm(data = partitionedData).algorithm())
    }).maxBy(_._2)
    println(s"The best combination: ${theBest._1.mkString(", ")} percent: ${theBest._2}")
  }

  private def testNN(): Unit = {
    val testNNData = PartitionedData(
      trainingData = Iterable(
        ObjectClass("A", Map("C1" -> 1, "C2" -> 2, "C3" -> 3)),
        ObjectClass("A", Map("C1" -> 2, "C2" -> 5, "C3" -> 3)),
        ObjectClass("A", Map("C1" -> 2, "C2" -> 5, "C3" -> 4)),
        ObjectClass("A", Map("C1" -> 4, "C2" -> 7, "C3" -> 5)),
        ObjectClass("B", Map("C1" -> 1, "C2" -> 7, "C3" -> 1)),
        ObjectClass("B", Map("C1" -> 3, "C2" -> 9, "C3" -> 1)),
        ObjectClass("B", Map("C1" -> 3, "C2" -> 8, "C3" -> 2)),
        ObjectClass("B", Map("C1" -> 5, "C2" -> 6, "C3" -> 1))
      ),
      testingData = Iterable(
        ObjectClass("B", Map("C1" -> 1, "C2" -> 7, "C3" -> 3))
      )
    )

    new NearestNeighborAlgorithm(testNNData).algorithm()
  }
}
