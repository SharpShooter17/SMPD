package smpd.service

import breeze.linalg.{DenseMatrix, det}
import smpd.domain.{Characteristic, Data}

object FisherExtractionService {

  def extraction(data: Data, countOfCharacteristics: Int): Data = {
    val averageOfCharacteristicsByClass = classAverage(data)
    val samplesAveragesByCharacteristic = samplesAverage(data)
    val characteristics = data.headOption.toList.flatMap(_._2).headOption.map(_.characteristics).toList.flatMap(_.keys)
    val characteristicsCombinations = characteristics.combinations(countOfCharacteristics).toList
    val fisherFactors = characteristicsCombinations.map(combination =>
      (combination, fisherFactor(combination, data, averageOfCharacteristicsByClass, samplesAveragesByCharacteristic))
    )

    data
  }

  private def fisherFactor(characteristics: List[String],
                           data: Data,
                           averageOfCharacteristicsByClass: Map[String, Characteristic],
                           samplesAveragesByCharacteristic: Map[String, Double]): Double = {
    val distance = vectorDistance(averageOfCharacteristicsByClass, samplesAveragesByCharacteristic, characteristics)
    val interClassSpread = countInterClassSpread(data, characteristics, averageOfCharacteristicsByClass)
    distance / interClassSpread
  }

  private def countInterClassSpread(data: Data,
                                    characteristics: List[String],
                                    averageOfCharacteristicsByClass: Map[String, Characteristic]): Double = {

  }

  private def vectorDistance(averageOfCharacteristicsByClass: Map[String, Characteristic],
                             samplesAveragesByCharacteristic: Characteristic,
                             characteristics: List[String]): Double = {
    val averageByClass: Map[String, Vector[Double]] = averageOfCharacteristicsByClass.map {
      case (className, ch) =>
        val mappedSamples =  filterCharacteristics(characteristics, ch).map(_._2)
        (className, mappedSamples)
    }

    val averages = filterCharacteristics(characteristics, samplesAveragesByCharacteristic).map(_._2)
    val averagesMatrix = DenseMatrix(averages: _*)

    val sum = averageByClass.map {
      case (_, matrix) =>
        val diff = DenseMatrix(matrix: _*) - averagesMatrix
        diff * diff.t
    }.reduce(_ + _)

    det(sum)
  }

  private def filterCharacteristics(filter: List[String], characteristic: Characteristic) : Vector[(String, Double)] = {
    characteristic.filter { case (name, _) => filter.contains(name) }.toVector.sortBy(_._1)
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
