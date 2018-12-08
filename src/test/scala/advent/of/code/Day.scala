package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

abstract class Day(val dayNumber: Int) extends FlatSpec with Matchers {
  val source                   = Source.fromFile(s"inputs/Day$dayNumber.txt")
  lazy val lines: List[String] = source.getLines().toList
  lazy val raw: String         = source.mkString
}
