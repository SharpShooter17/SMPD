package smpd

package object domain {

  /**
   * Name of characteristics, value
   */
  type Characteristic = Map[String, Double]

  /**
   * Name of class, samples
   */
  type Data = Map[String, Vector[ObjectClass]]

}
