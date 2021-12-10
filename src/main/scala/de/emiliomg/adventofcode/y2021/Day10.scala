package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex
import scala.collection.mutable.Stack

object Day10 {

  def star1(data: List[String]): Long = {
    data
      .map(LineType.fromLine)
      .collect { case l: CorruptedLine => l }
      .map(_.firstCorrupted)
      .map {
        _ match {
          case ')' => 3
          case ']' => 57
          case '}' => 1197
          case '>' => 25137
        }
      }
      .sum
  }

  def star2(data: List[String]): Long = {
    val listOfPoints = data
      .map(LineType.fromLine)
      .collect { case l: IncompleteLine => l }
      .map { incompleteLine =>
        incompleteLine.remainingStack.foldLeft(0L) { (score, c) =>
          val newPoints: Long = c match {
            case '(' => 1
            case '[' => 2
            case '{' => 3
            case '<' => 4
            case omg => throw Exception(s"Got $omg, this should not happen")
          }
          (score * 5) + newPoints
        }
      }
      .sorted

    listOfPoints(listOfPoints.size / 2)
  }

  object OpeningChar {
    val allowedChars = List('(', '<', '[', '{')

    def unapply(c: Char): Boolean = {
      if allowedChars.contains(c) then true
      else false
    }
  }

  object ClosingChar {
    val allowedChars = List(')', '>', ']', '}')

    def unapply(c: Char): Boolean = {
      if allowedChars.contains(c) then true
      else false
    }
  }

  object ValidMatch {
    def unapply(chars: (Char, Char)): Boolean = chars match {
      case ('(', ')') => true
      case ('[', ']') => true
      case ('{', '}') => true
      case ('<', '>') => true
      case _          => false
    }
  }

  trait LineType
  case class CorruptedLine(val firstCorrupted: Char)         extends LineType
  case class IncompleteLine(val remainingStack: Stack[Char]) extends LineType

  object LineType {
    def fromLine(line: String): LineType = {
      val stack: Stack[Char] = Stack.empty

      def step(toProcess: List[Char]): LineType = {
        toProcess.headOption match {
          case None if stack.nonEmpty => IncompleteLine(stack)
          case Some(char @ OpeningChar()) =>
            stack.push(char)
            step(toProcess.tail)
          case Some(char @ ClosingChar()) if stack.isEmpty =>
            CorruptedLine(char)
          case Some(char @ ClosingChar()) =>
            (stack.pop, char) match {
              case ValidMatch() => step(toProcess.tail)
              case _            => CorruptedLine(char)
            }
          case omg => throw Exception(s"Got $omg, this should not happen")
        }
      }

      step(line.toCharArray.toList)
    }
  }
}
