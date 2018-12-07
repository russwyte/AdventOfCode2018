package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

abstract class Day(val dayNumber: Int) extends FlatSpec with Matchers {
  lazy val input: List[String] = Source.fromFile(s"inputs/Day$dayNumber.txt").getLines().toList
}
