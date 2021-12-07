package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day7Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day7.star1(getTestInput) shouldEqual 37
  }

  it should "work with the puzzle input" in {
    val result = Day7.star1(getPuzzleInput)
    pprint.pprintln(s"Day7, Star1: $result")
    result shouldEqual 351901
  }

  // "Star 2" should "work with the test input" in {
  //   Day7.star2(getTestInput) shouldEqual 26984457539L
  // }

  // it should "work with the puzzle input" in {
  //   val result = Day7.star2(getPuzzleInput)
  //   pprint.pprintln(s"Day7, Star2: $result")
  //   // result shouldEqual 1749945484935L
  // }

  private def getTestInput: List[String] = {
    """16,1,2,0,4,2,7,1,2,14""".stripMargin.split("\n").toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day7.txt").getLines().toList
  }
}
