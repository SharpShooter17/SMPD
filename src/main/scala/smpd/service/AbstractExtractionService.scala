package smpd.service

import breeze.linalg.{DenseMatrix, det}
import smpd.domain.{Characteristic, Data, ObjectClass}

import scala.collection.mutable.ListBuffer

abstract class AbstractExtractionService(data: Data) {

  protected val averageOfCharacteristicsByClass: Map[String, Characteristic] = classAverage(data)
  protected val samplesAveragesByCharacteristic: Characteristic = samplesAverage(data)

  protected def extraction(countOfCharacteristics: Int): Data

  def extract(countOfCharacteristics: Int): Data = {
    validateCharacteristicsSize(countOfCharacteristics)
    extraction(countOfCharacteristics)
  }

  protected def fisherFactor(characteristics: Set[String]): Double = {
    val distance = vectorDistance(characteristics)
    val interClassSpread = countInterClassSpread(characteristics)
    distance / interClassSpread
  }

  def filterCharacteristics(filter: Set[String], characteristic: Characteristic): Characteristic = {
    characteristic.filter { case (name, _) => filter.contains(name) }
  }

  protected def remapData(characteristics: Set[String]) : Data = {
    data.map {
      case (className, samples) =>
        val mappedSamples = samples.map(objectClass => objectClass.copy(characteristics = filterCharacteristics(characteristics, objectClass.characteristics)))
        (className, mappedSamples)
    }
  }

  private def countInterClassSpread(characteristics: Set[String]): Double = {
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

  private def vectorDistance(characteristics: Set[String]): Double = {
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

  private def extractData(filter: Set[String], characteristic: Characteristic): Vector[Double] = {
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

  protected def validateCharacteristicsSize(countOfCharacteristics: Int): Unit = {
    val size = listOfCharacteristics().size
    if (countOfCharacteristics < 0 || size < countOfCharacteristics) {
      throw new IllegalArgumentException(s"Count of characteristics should be in range 1 to $size")
    }
  }

  protected def listOfCharacteristics() : List[String] = {
    data.headOption.toList.flatMap(_._2).headOption.map(_.characteristics).toList.flatMap(_.keys)
  }

}
