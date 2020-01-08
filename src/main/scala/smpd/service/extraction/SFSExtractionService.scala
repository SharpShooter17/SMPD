package smpd.service.extraction

import smpd.domain.Data

class SFSExtractionService(data: Data) extends AbstractExtractionService(data) {

  override protected def extraction(countOfCharacteristics: Int): Data = {
    val characteristics = findCharacteristics(countOfCharacteristics)
    println(s"Selected characteristics: ${characteristics.mkString(", ")}")
    remapData(characteristics)
  }

  @scala.annotation.tailrec
  private def findCharacteristics(countOfCharacteristics: Int, characteristics: Set[String] = Set.empty): Set[String] = {
    if (countOfCharacteristics == characteristics.size) {
      characteristics
    } else {
      val availableCharacteristics = listOfCharacteristics().toSet -- characteristics
      val theBestCharacteristics = availableCharacteristics.map(characteristic => (characteristic, fisherFactor(characteristics + characteristic)))
        .maxBy(_._2)._1
      findCharacteristics(countOfCharacteristics, characteristics + theBestCharacteristics)
    }
  }

}
