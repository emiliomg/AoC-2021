package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day10Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day10.star1(getTestInput) shouldEqual 26397
  }

  it should "work with the puzzle input" in {
    val result = Day10.star1(getPuzzleInput)
    pprint.pprintln(s"Day10, Star1: $result")
    result shouldEqual 370407
  }

  "Star 2" should "work with the test input" in {
    Day10.star2(getTestInput) shouldEqual 288957
  }

  it should "work with the puzzle input" in {
    val result = Day10.star2(getPuzzleInput)
    pprint.pprintln(s"Day10, Star2: $result")
    result shouldEqual 3249889609L
  }

  private def getTestInput: List[String] =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\n").toList

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day10.txt").getLines().toList
  }
}
