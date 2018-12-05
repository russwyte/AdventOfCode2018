package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Day5 extends FlatSpec with Matchers with Day {
  override def dayNumber: Int = 5

  @tailrec
  final def part1(x: String): Int = {
    val r = x.foldLeft("") { (s, c) =>
      if (s.isEmpty) s + c
      else if (Math.abs(s.last - c) == 32) {
        s.init
      } else s + c
    }
    if (r.length != x.length) part1(r) else r.length
  }

  def part2(s: String): Int =
    s.toLowerCase.distinct.map { c =>
      part1(s.filterNot(x => x.toLower == c))
    }.min

  "reaction" should "react" in {
    part1("dabAcCaCBAcCcaDA") should be(10)
    part1(this.input.head) should be(10450)
    part2("dabAcCaCBAcCcaDA") should be(4)
    part2(this.input.head) should be(4624)
  }
}
