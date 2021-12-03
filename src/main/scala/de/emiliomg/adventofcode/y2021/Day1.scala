package de.emiliomg.adventofcode.y2021

object Day1 {
  def star1(data: List[Int]): Int = {
    case class Step(cnt: Int, prev: Option[Int])
    val result: Step = data.foldLeft(Step(0, None)) {
      case (Step(cnt, None), elem)                      => Step(cnt, Some(elem))
      case (Step(cnt, Some(prev)), elem) if elem > prev => Step(cnt + 1, Some(elem))
      case (Step(cnt, _), elem)                         => Step(cnt, Some(elem))
    }
    result.cnt
  }

  def star2(data: List[Int]): Int = {
    val processedData = data.sliding(3).map(_.sum).toList
    star1(processedData)
  }
}
