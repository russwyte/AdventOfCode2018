package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Day5 extends FlatSpec with Matchers with Day {
  override def dayNumber: Int = 5

  @tailrec
  final def p1(x: String): Int = {
    val r = x.foldLeft("")((s, c) => if (s.isEmpty || Math.abs(s.last - c) != 32) s + c else s.init)
    if (r != x) p1(r) else r.length
  }

  def p2(s: String): Int = s.toLowerCase.distinct.map(c => p1(s.filter(_.toLower != c))).min

  "reaction" should "react" in {
    p1("dabAcCaCBAcCcaDA") should be(10)
    p1(this.input.head) should be(10450)
    p2("dabAcCaCBAcCcaDA") should be(4)
    p2(this.input.head) should be(4624)
  }
}
