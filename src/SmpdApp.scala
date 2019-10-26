import domain.{ObjectClass, PartitionedData}
import service.{AverageOfNearestNeighborAlgorithm, NearestNeighborAlgorithm, SeveralNearestNeighborAlgorithm}

import scala.io.Source._

object SmpdApp extends App {

  val rawData = loadDataFromFile("data.txt")
  val data = PartitionedData(rawData, 0.80)

  println("NearestNeighborAlgorithm-------------------------------------------------------------------------------------")
  new NearestNeighborAlgorithm(data).algorithm()

  println("SeveralNearestNeighborAlgorithm-k=1--------------------------------------------------------------------------")
  new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 1).algorithm()

  println("SeveralNearestNeighborAlgorithm-k=2--------------------------------------------------------------------------")
  new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 2).algorithm()

  println("SeveralNearestNeighborAlgorithm-k-=3-------------------------------------------------------------------------")
  new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 3).algorithm()

  println("SeveralNearestNeighborAlgorithm-k=5--------------------------------------------------------------------------")
  new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 6).algorithm()

  println("SeveralNearestNeighborAlgorithm-k=10-------------------------------------------------------------------------")
  new SeveralNearestNeighborAlgorithm(data = data, countOfNeighborToMatch = 10).algorithm()

  println("AverageOfNearestNeighborAlgorithm----------------------------------------------------------------------------")
  new AverageOfNearestNeighborAlgorithm(data = data).algorithm()


  private def loadDataFromFile(filename: String): Map[String, Vector[ObjectClass]] = {
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
