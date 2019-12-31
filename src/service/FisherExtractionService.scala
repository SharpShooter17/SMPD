package service

import domain.{Characteristic, Data}

object FisherExtractionService {

  def extraction(data: Data, countOfCharacteristics: Int): Data = {
    val averageOfCharacteristicsByClass = classAverage(data)
    val samplesAveragesByCharacteristic = samplesAverage(data)
    val characteristics = data.headOption.toList.flatMap(_._2).headOption.map(_.characteristics).toList.flatMap(_.keys)
    val characteristicsCombinations = characteristics.combinations(countOfCharacteristics).toList
    val fisherFactors = characteristicsCombinations.map(combination => (combination, fisherFactor(combination, data, averageOfCharacteristicsByClass)))


    data
  }

  private def fisherFactor(characteristics: List[String], data: Data, averageOfCharacteristicsByClass: Map[String, Characteristic]): Double = {
    val distance = vectorDistance(data, averageOfCharacteristicsByClass, characteristics)
  }

  private def vectorDistance(data: Data, averageOfCharacteristicsByClass: Map[String, Characteristic], characteristics: List[String]): Double = {
    characteristics
  }

  private def classAverage(data: Data): Map[String, Characteristic] = {
    data.map {
      case (clazz, objectClass) =>
        val sum = sumSamplesByCharacteristics(objectClass.map(_.characteristics))
        val countOfSamples = data(clazz).size
        val averages = sum.map {
          case (characteristic, value) => (characteristic, value / countOfSamples)
        }
        (clazz, averages)
    }
  }

  private def samplesAverage(data: Data): Map[String, Double] = {
    val samples = data.flatMap(_._2).map(_.characteristics)
    val countOfSamples = samples.size
    val sum = sumSamplesByCharacteristics(samples)
    sum.map {
      case (characteristic, value) => (characteristic, value / countOfSamples)
    }
  }

  private def sumSamplesByCharacteristics(samples: Iterable[Characteristic]): Characteristic = {
    samples.reduce {
      (c1, c2) =>
        c1.map {
          case (key, value) =>
            (key, value + c2(key))
        }
    }
  }


}
