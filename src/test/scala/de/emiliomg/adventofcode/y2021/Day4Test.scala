package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day4Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day4.star1(getTestInput) shouldEqual 4512
  }

  it should "work with the puzzle input" in {
    val result = Day4.star1(getPuzzleInput)
    pprint.pprintln(s"Day4, Star1: $result")
    // result shouldEqual 1488669
  }

  // "Star 2" should "work with the test input" in {
  //   Day4.star2(getTestInput) shouldEqual 230
  // }

  // it should "work with the puzzle input" in {
  //   val result = Day4.star2(getPuzzleInput)
  //   pprint.pprintln(s"Day4, Star2: $result")
  //   // result shouldEqual 1176514794
  // }

  private def getTestInput: List[String] = {
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin.split("\n").toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day4.txt").getLines().toList
  }
}