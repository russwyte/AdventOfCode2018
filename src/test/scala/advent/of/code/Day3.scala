package advent.of.code

import advent.of.code.Day3.{Claim, Point}
import fastparse.NoWhitespace._
import fastparse._
import org.scalatest.{FlatSpec, Matchers}

object Day3 extends Day {
  override def dayNumber: Int = 3

  object Parser {
    // #10 @ 981,813: 15x14
    def pID[_: P]: P[Int] = P("#" ~ pUnsignedInt)

    def pPos[_: P]: P[Point] = P(pUnsignedInt ~ "," ~ pUnsignedInt).map { case (x, y) => Point(x, y) }

    def pWH[_: P]: P[(Int, Int)] = P(pUnsignedInt ~ "x" ~ pUnsignedInt)

    def pClaim[_: P]: P[Claim] = P(pID ~ " @ " ~ pPos ~ ": " ~ pWH).map {
      case (id, p: Point, (lx, ly)) => Claim(id, p, lx, ly)
    }
  }

  case class Claim(id: Int, pos: Point, width: Int, height: Int) {
    def area: List[Point] = {
      (for {
        x <- pos.x until (pos.x + width)
        y <- pos.y until (pos.y + height)
      } yield {
        Point(x, y)
      }).toList
    }
  }

  case class Point(x: Int, y: Int)

  def part1(claims: Claim*): Int = {
    claims.flatMap(x => x.area).groupBy(identity).map(_._2.size).count(x => x > 1)
  }

  def part2(claims: Claim*): Claim = {
    case class Context(set: Set[Claim], cloth: Map[Point, Claim]) {
      def addPoint(p: Point, claim: Claim): Context =
        copy(if (cloth.get(p).exists(_ != claim)) set -- Seq(cloth(p), claim) else set, cloth.updated(p, claim))
    }
    claims
      .foldLeft(Context(Set(claims: _*), Map.empty)) {
        case (ctx, claim) =>
          claim.area.foldLeft(ctx) { case (ctx2, p) => ctx2.addPoint(p, claim) }
      }
      .set
      .head
  }
}

class Day3Specs extends FlatSpec with Matchers {
  val rs = List(
    Claim(1, Point(1, 3), 4, 4),
    Claim(2, Point(3, 1), 4, 4),
    Claim(3, Point(5, 5), 2, 2),
  )

  val claims = Day3.input.map(parse(_, Day3.Parser.pClaim(_)).get.value)
  "part1" should "work" in {
    Day3.part1(rs: _*) should be(4)
    Day3.part1(claims: _*) should be(118858)
  }
  "part2" should "work" in {
    Day3.part2(claims: _*).id should be(1100)
  }
}
