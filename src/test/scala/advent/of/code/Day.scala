package advent.of.code

import scala.io.Source

trait Day {
  def dayNumber: Int

  lazy val input: List[String] = Source.fromFile(s"inputs/Day$dayNumber.txt").getLines().toList
}
