package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex
import scala.collection.mutable

object Day8 {
  def star1(data: List[String]): Long = {
    data.map { entry =>
      val Array(_, digitStr) = entry.split("\\|").map(_.trim)
      val digits             = digitStr.split(" ").map(_.trim)
      digits.filter(digit => List(2, 4, 3, 7).contains(digit.size)).size
    }.sum
  }

  def star2(data: List[String]): Long = {
    data.map { entryStr =>
      Entry.fromEntryString(entryStr).digitsAsNum
    }.sum
  }

  def parseEntry(str: String): Entry = ???

  case class Entry(
    signals: List[String],
    digits: List[String],
    digitStringMapping: Map[String, Int],
    digitsAsNum: Int
  )

  object Entry {
    def fromEntryString(entryString: String): Entry = {

      val Array(signals: List[String], digits: List[String]) =
        entryString.split("\\|").map(_.trim).map(_.split(" ").map(_.trim).toList)

      val signalSizeToSignalString: Map[Int, List[String]] = signals.groupBy(_.size)

      // mutable, hm
      val signalStringToDigit: mutable.Map[String, Int] = mutable.Map.empty
      val digitToSignalstring: mutable.Map[Int, String] = mutable.Map.empty

      def storeSignalsToDigit(sizeToSearchFor: Int, f: String => Int): Unit = {
        val segmentStrings = signalSizeToSignalString(sizeToSearchFor)

        segmentStrings.foreach { segmentString =>
          val sortedSegmentString = segmentString.sorted
          val digit               = f(sortedSegmentString)

          signalStringToDigit.update(sortedSegmentString, digit)
          digitToSignalstring.update(digit, sortedSegmentString)
        }
      }

      /*
        2 segments -> 1
        3 segments -> 7
        4 segments -> 4
        5 segments -> 3,5,2
        6 segments -> 6,9,0
        7 segments -> 8
       */

      // unique ones
      storeSignalsToDigit(2, _ => 1)
      storeSignalsToDigit(3, _ => 7)
      storeSignalsToDigit(4, _ => 4)
      storeSignalsToDigit(7, _ => 8)

      // some deduction, literal "pattern matching" on the digits 🤪
      storeSignalsToDigit(
        5,
        _ match {
          case three if three.intersect(digitToSignalstring(1)).size == 2 => 3
          case five if five.intersect(digitToSignalstring(4)).size == 3   => 5
          case _                                                          => 2
        }
      )

      storeSignalsToDigit(
        6,
        _ match {
          case six if six.intersect(digitToSignalstring(1)).size == 1   => 6
          case nine if nine.intersect(digitToSignalstring(4)).size == 4 => 9
          case _                                                        => 0
        }
      )

      val num = digits.map(digit => signalStringToDigit(digit.sorted)).mkString.toInt

      // pprint.pprintln(signalStringToDigit)

      Entry(signals, digits, signalStringToDigit.toMap, num)
    }
  }
}
