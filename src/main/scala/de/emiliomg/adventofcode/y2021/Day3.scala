package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day3 {

  def star1(data: List[String]): Int = {
    val gammaValues = data.transpose.map { (row: List[Char]) =>
      if (row.filter(_ == '1').length > (row.length / 2))
        1
      else
        0
    }

    val epsilonValues = gammaValues.map {
      case 1   => 0
      case 0   => 1
      case omg => throw Exception(s"received $omg, this should not happen")
    }

    val gamma   = Integer.parseInt(gammaValues.mkString, 2)
    val epsilon = Integer.parseInt(epsilonValues.mkString, 2)

    gamma * epsilon
  }

  def star2(data: List[String]): Int = {

    def step(dataToConsider: List[String], pos: Int, comp: (Int, Float) => Boolean): String = {
      val numbersInPosition = dataToConsider.transpose.apply(pos)
      val choice =
        if comp(numbersInPosition.filter(_ == '1').size, (numbersInPosition.size.toFloat / 2)) then '1' else '0'
      val survivorPositions = numbersInPosition.zipWithIndex.partition(_._1 == choice)._1.map(_._2)
      val survivors         = survivorPositions.map(dataToConsider(_))

      // println("########################")
      // pprint.pprintln(s"dataToConsider: $dataToConsider")
      // pprint.pprintln(s"pos: $pos")
      // pprint.pprintln(s"numbersInPosition: $numbersInPosition")
      // pprint.pprintln(s"choice: $choice")
      // pprint.pprintln(s"survivorPositions: $survivorPositions")
      // pprint.pprintln(s"survivors: $survivors")
      // println("########################")

      if (survivors.size == 1) survivors.head
      else step(survivors, pos + 1, comp)
    }

    val oxyString = step(data, 0, _ >= _)
    val oxy       = Integer.parseInt(oxyString, 2)

    val co2String = step(data, 0, _ < _)
    val co2       = Integer.parseInt(co2String, 2)

    oxy * co2
  }
}
