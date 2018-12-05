package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Day5 extends FlatSpec with Matchers with Day {

  override def dayNumber: Int = 5

  def willReact(a: Char, b: Char): Boolean = Math.abs(a - b) == 32


  @tailrec
  final def part1(s: String):Int = {
    val res = s.foldLeft(""){case (s,c) =>
      if (s.isEmpty) s + c
      else if (willReact(s.last,c)) {
        s.init
      }
      else s + c
    }
    if (res != s) part1(res) else res.length
  }

  def part2(s: String): Int = {
    s.toLowerCase.distinct.sorted
      .map { c =>
        c -> part1(s.filterNot(x => x.toLower == c))
      }
      .toMap
      .minBy(_._2)
      ._2
  }

  "reaction" should "react" in {
    part1("dabAcCaCBAcCcaDA") should be(10)
    part1(this.input.head) should be(10450)
    part2("dabAcCaCBAcCcaDA") should be(4)
    part2(this.input.head) should be(4624)
  }
}
