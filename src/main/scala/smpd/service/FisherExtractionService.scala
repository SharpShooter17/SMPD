package smpd.service

import smpd.domain.Data

class FisherExtractionService(data: Data) extends AbstractExtractionService(data) {

  override protected def extraction(countOfCharacteristics: Int): Data = {
    val characteristics = listOfCharacteristics()
    val characteristicsCombinations = characteristics.combinations(countOfCharacteristics).toList
    val fisherFactors = characteristicsCombinations.map(combination =>
      (combination, fisherFactor(combination.toSet))
    )

    val (bestCharacteristics, factor) = fisherFactors.maxBy(_._2)
    println(s"The Best Combination\n\tFisher factor: $factor\n\tFor characteristics: ${bestCharacteristics.mkString(", ")}")

    remapData(bestCharacteristics.toSet)
  }

}
