package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day5Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day5.star1(getTestInput) shouldEqual 5
  }

  it should "work with the puzzle input" in {
    val result = Day5.star1(getPuzzleInput)
    pprint.pprintln(s"Day5, Star1: $result")
    result shouldEqual 5576
  }

  // "Star 2" should "work with the test input" in {
  //   Day5.star2(getTestInput) shouldEqual 12
  // }

  // it should "work with the puzzle input" in {
  //   val result = Day5.star2(getPuzzleInput)
  //   pprint.pprintln(s"Day5, Star2: $result")
  //   // result shouldEqual 3178
  // }

  private def getTestInput: List[String] = {
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin.split("\n").toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day5.txt").getLines().toList
  }
}
