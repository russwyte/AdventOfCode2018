package advent.of.code
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Day14 extends FlatSpec with Matchers {
  case class Board(pos1: Int = 0, pos2: Int = 1, recipes: Vector[Int] = Vector(3, 7)) {
    def tick = {
      val r1  = recipes(pos1)
      val r2  = recipes(pos2)
      val sum = r1 + r2
      val rs  = recipes ++ (if (sum < 10) Vector(sum) else Vector(sum / 10, sum % 10))
      copy(pos1 = (pos1 + r1 + 1) % rs.length, pos2 = (pos2 + r2 + 1) % rs.length, recipes = rs)
    }
    def part1(n: Int): String = {
      @tailrec
      def inner(b: Board): Board = {
        if (b.recipes.length >= 10 + n) b else inner(b.tick)
      }
      inner(this).recipes.splitAt(n)._2.take(10).mkString
    }
    // I am not crazy about this solution - it is rather slow
    def part2(s: String): Int = {
      @tailrec
      def inner(b: Board): Int = {
        val rs = b.recipes.takeRight(s.length + 12).mkString
        if (rs.contains(s)) {
          b.recipes.mkString.indexOf(s)
        } else {
          inner(b.tick.tick.tick.tick.tick.tick)
        }
      }
      inner(this)
    }
  }

  "Part 1" should "work" in {
    Board().part1(9) should be("5158916779")
    Board().part1(5) should be("0124515891")
    Board().part1(18) should be("9251071085")
    Board().part1(2018) should be("5941429882")
    Board().part1(30121) should be("5101271252")
  }
  "Part2" should "work" in {
    Board().part2("51589") should be(9)
    Board().part2("030121") should be(20287556)
  }
}
