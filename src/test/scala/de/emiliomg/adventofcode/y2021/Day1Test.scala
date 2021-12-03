package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day1Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day1.star1(getTestInput) shouldEqual 7
  }

  it should "work with the puzzle input" in {
    val result = Day1.star1(getPuzzleInput)
    pprint.pprintln(s"Day1, Star1: $result")
    result shouldEqual 1711
  }

  "Star 2" should "work with the test input" in {
    Day1.star2(getTestInput) shouldEqual 5
  }

  it should "work with the puzzle input" in {
    val result = Day1.star2(getPuzzleInput)
    pprint.pprintln(s"Day2, Star2: $result")
    result shouldEqual 1743
  }

  private def getTestInput: List[Int] = {
    List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
  }

  private def getPuzzleInput: List[Int] = {
    Source.fromResource("day1.txt").getLines().toList.map(_.toInt)
  }
}
