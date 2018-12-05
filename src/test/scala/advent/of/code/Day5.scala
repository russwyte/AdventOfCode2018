package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Day5 extends FlatSpec with Matchers with Day {

  override def dayNumber: Int = 5

  def willReact(a: Char, b: Char): Boolean = Math.abs(a - b) == 32

  def stream(l: List[Char]): Stream[Char] = {
    l match {
      case Nil => Stream.empty
      case a :: Nil => a #:: stream(Nil)
      case a :: b :: xs =>
        if (willReact(a, b)) {
          stream(xs)
        } else {
          a #:: stream(b :: xs)
        }
    }
  }

  // I could optimize this - but it is fast enough
  @tailrec
  final def part1(s: String): Int = {
    val res = stream(s.toList).mkString
    if (res == s) res.length else part1(res)
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
