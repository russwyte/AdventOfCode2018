package advent.of.code
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Day11 extends FlatSpec with Matchers {
  val SerialNumber = 7400
  case class CellResult(p: Point, size: Int, total: Int)

  def power(p: Point, sn: Int): Int = {
    val rackID: Int = p.x + 10
    (((((rackID * p.y) + sn) * rackID) / 100) % 10) - 5
  }

  case class Summer(min: Point, max: Point, sn: Int) {
    val sums = mutable.Map[Point, Int]().withDefaultValue(0)
    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val p   = Point(x, y)
        val sum = sums(p + Point(0, -1)) + sums(p + Point(-1, 0)) - sums(p + Point(-1, -1)) + power(p, sn)
        sums.put(p, sum)
      }
    }
    def apply(tl: Point, br: Point): Int = {
      sums(br) - sums(Point(tl.x - 1, br.y)) - sums(Point(br.x, tl.y - 1)) + sums(Point(tl.x - 1, tl.y - 1))
    }
  }

  def part1(s: Int): Point = {
    val summer = Summer(Point(1, 1), Point(300, 300), s)
    (for {
      y <- 1 to 297
      x <- 1 to 297
    } yield {
      val p1 = Point(x, y)
      val p2 = p1 + Point(2, 2)
      p1 -> summer(p1, p2)
    }).toMap.maxBy(_._2)._1
  }

  def part2(s: Int): CellResult = {
    val summer = Summer(Point(1, 1), Point(300, 300), s)
    (for {
      size <- 1 to 300
      y    <- 1 to (300 - size)
      x    <- 1 to (300 - size)
    } yield {
      val p   = Point(x, y)
      val p2  = p + Point(size - 1, size - 1)
      val sum = summer(p, p2)
      CellResult(p, size, sum)
    }).maxBy(_.total)
  }

  "A cell" should "calculate power" in {
    power(Point(3, 5), 8) should be(4)
    power(Point(122, 79), 57) should be(-5)
    power(Point(217, 196), 39) should be(0)
    power(Point(101, 153), 71) should be(4)
  }
  "Part 1" should "work" in {
    part1(18) should be(Point(33, 45))
    part1(42) should be(Point(21, 61))
    part1(SerialNumber) should be(Point(34, 72))
  }
  "Part 2" should "work" in {
    part2(7400) should be(CellResult(Point(233, 187), 13, 91))
  }
}
