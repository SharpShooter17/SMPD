package smpd.service

import breeze.linalg.{DenseMatrix, det, eig}
import smpd.domain.{Characteristic, Data, ObjectClass}

import scala.collection.mutable.ListBuffer

object FisherExtractionService {

  def extraction(data: Data, countOfCharacteristics: Int): Data = {
    val averageOfCharacteristicsByClass = classAverage(data)
    val samplesAveragesByCharacteristic = samplesAverage(data)
    val characteristics = data.headOption.toList.flatMap(_._2).headOption.map(_.characteristics).toList.flatMap(_.keys)
    val characteristicsCombinations = characteristics.combinations(countOfCharacteristics).toList
    val fisherFactors = characteristicsCombinations.map(combination =>
      (combination, fisherFactor(combination, data, averageOfCharacteristicsByClass, samplesAveragesByCharacteristic))
    )
    val (bestCharacteristics, factor) = fisherFactors.maxBy(_._2)
    println(s"The Best Combination\n\tFisher factor: $factor\n\tFor characteristics: ${bestCharacteristics.mkString(", ")}")

    data.map {
      case (className, samples) =>
        val mappedSamples = samples.map(objectClass => objectClass.copy(characteristics = filterCharacteristics(bestCharacteristics, objectClass.characteristics)))
        (className, mappedSamples)
    }
  }

  private def fisherFactor(characteristics: List[String],
                           data: Data,
                           averageOfCharacteristicsByClass: Map[String, Characteristic],
                           samplesAveragesByCharacteristic: Characteristic): Double = {
    val distance = vectorDistance(data, averageOfCharacteristicsByClass, samplesAveragesByCharacteristic, characteristics)
    val interClassSpread = countInterClassSpread(data, characteristics, averageOfCharacteristicsByClass)
    distance / interClassSpread
  }

  private def countInterClassSpread(data: Data,
                                    characteristics: List[String],
                                    averageOfCharacteristicsByClass: Map[String, Characteristic]): Double = {
    val filteredData = data.map {
      case (className, samples) =>
        val mappedSamples = samples.map {
          case ObjectClass(_, ch) =>
            extractData(characteristics, ch)
        }
        (className, mappedSamples)
    }

    filteredData.map {
      case (className, samples) =>
        val classMatrix = DenseMatrix(samples: _*).t
        val average = extractData(characteristics, averageOfCharacteristicsByClass(className))
        val matrixOfAverageBuilder = new ListBuffer[Vector[Double]]
        for (_ <- 1 to classMatrix.cols) {
          matrixOfAverageBuilder.addOne(average)
        }
        val averageMatrix = DenseMatrix(matrixOfAverageBuilder.toVector: _*).t
        val diff = classMatrix - averageMatrix
        val multiply = diff * diff.t
        det(multiply)
    }.sum
  }

  private def vectorDistance(data: Data,
                              averageOfCharacteristicsByClass: Map[String, Characteristic],
                             samplesAveragesByCharacteristic: Characteristic,
                             characteristics: List[String]): Double = {
    val averageByClass: Map[String, Vector[Double]] = averageOfCharacteristicsByClass.map {
      case (className, ch) =>
        val mappedSamples = extractData(characteristics, ch)
        (className, mappedSamples)
    }

    val averages = extractData(characteristics, samplesAveragesByCharacteristic)
    val averagesMatrix = DenseMatrix(averages: _*)

    val sum = averageByClass.map {
      case (className, matrix) =>
        val diff = DenseMatrix(matrix: _*) - averagesMatrix
        val multi = diff * diff.t
        multi * data(className).size.toDouble
    }.reduce(_ + _)

    det(sum)
  }

  private def filterCharacteristics(filter: List[String], characteristic: Characteristic): Characteristic = {
    characteristic.filter { case (name, _) => filter.contains(name) }
  }

  private def extractData(filter: List[String], characteristic: Characteristic): Vector[Double] = {
    filterCharacteristics(filter, characteristic).toVector.sortBy(_._1).map(_._2)
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

  private def samplesAverage(data: Data): Characteristic = {
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
