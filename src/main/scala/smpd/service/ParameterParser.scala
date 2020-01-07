package smpd.service

import smpd.domain.Parameters

object ParameterParser {

  private val availableAlgorithms = List("NN", "SNN", "ANN", "SANN")

  def parseArgs(args: List[String]) : Parameters = {
    val map = nextOption(Map.empty, args)
    Parameters(
      file = map.getOrElse("-file", throw new IllegalArgumentException("Cannot find -file argument")),
      k = map.getOrElse("-k", "1").toInt,
      algorithm = map.getOrElse("-algorithm", throw new IllegalArgumentException("Cannot find -algorithm argument")),
      testing = map.getOrElse("-testing", "0.25").toDouble,
      characteristics = map.getOrElse("-ch", "3").toInt
    )
  }

  @scala.annotation.tailrec
  private def nextOption(options: Map[String, String], args: List[String]) : Map[String, String] = {
      args match {
        case Nil => options
        case "-file" :: fileName :: tail => nextOption(options ++ Map("-file" -> fileName), tail)
        case "-algorithm" :: algorithm :: tail =>
          if (!availableAlgorithms.contains(algorithm)) {
            throw new IllegalArgumentException(s"Given algorithm does not exists: $algorithm")
          }
          nextOption(options ++ Map("-algorithm" -> algorithm), tail)
        case "-k" :: k :: tail => nextOption(options ++ Map("-k" -> k), tail)
        case "-testing" :: testing :: tail => nextOption(options ++ Map("-testing" -> testing), tail)
        case "-ch" :: value ::tail => nextOption(options ++ Map("-ch" -> value), tail)
        case "-help" :: tail =>
          showHelp()
          nextOption(options, tail)
        case value :: tail => throw new IllegalArgumentException(s"Unknown argument: $value")
      }
  }

  private def showHelp() : Unit = {
    println("Available arguments:")
    println("\t-file path/to/file.txt")
    println("\t-algorithm name of algorithm")
    println("\t\tNN - Nearest Neighbor")
    println("\t\tSNN - Several Nearest Neighbor")
    println("\t\tANN - Average Of Nearest Neighbor")
    println("\t\tANN - Several Average Of Nearest Neighbor")
    println("\t-testing X - percent of testing objects, default is set to 0.25")
    println("\t-k X - count of nearest neighbors, default is set to 1")
    println("\t-ch X - count of characteristics, default is set to 3")
  }

}
