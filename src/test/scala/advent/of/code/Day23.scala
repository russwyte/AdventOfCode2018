package advent.of.code
import fastparse._
import NoWhitespace._

import scala.annotation.tailrec

class Day23 extends Day(23) {
  val Sample1 = """pos=<0,0,0>, r=4
                 |pos=<1,0,0>, r=1
                 |pos=<4,0,0>, r=3
                 |pos=<0,2,0>, r=1
                 |pos=<0,5,0>, r=3
                 |pos=<0,0,3>, r=1
                 |pos=<1,1,1>, r=1
                 |pos=<1,1,2>, r=1
                 |pos=<1,3,1>, r=1""".stripMargin

  val Sample2 = """pos=<10,12,12>, r=2
                  |pos=<12,14,12>, r=2
                  |pos=<16,12,12>, r=4
                  |pos=<14,14,14>, r=6
                  |pos=<50,50,50>, r=200
                  |pos=<10,10,10>, r=5""".stripMargin

  case class Bot(p: Point, r: Int) {
    def inRange(b: Bot): Boolean        = (r + b.r) >= p.mDistance(b.p)
    def radiusContains(b: Bot): Boolean = (b.p mDistance p) <= r
  }

  def pBots[_: P]: P[Seq[Bot]] =
    P(
      "pos=<" ~ (pInt ~ "," ~ pInt ~ "," ~ pInt)
        .map { case (x, y, z) => Point(x, y, z, 0) } ~ ">, r=" ~ pUnsignedInt ~ "\n".?).map(Bot.tupled).rep

  def parseBots(s: String): Seq[Bot] = parse(s, pBots(_)).get.value

  def inRangeOfStrongest(s: String): Int = {
    val bots      = parseBots(s)
    val strongest = bots.maxBy(_.r)
    bots.count(b => strongest radiusContains b)
  }

  // use the Bron–Kerbosch algorithm to find the largest connected group in the disjoint graph
  // https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
  def largestGroup(neighbors: Map[Bot, Set[Bot]]): Set[Bot] = {
    // I suppose this could be made tail recursive - not bothering right now
    def inner(sR: Set[Bot], sP: Set[Bot], sX: Set[Bot], largest: Set[Bot]): Set[Bot] = {
      if (sP.isEmpty && sX.isEmpty) {
        if (sR.size > largest.size) sR else largest
      } else {
        // use the pivot version of the algorithm
        val u = (sP ++ sX).maxBy(b => neighbors(b).size)
        (sP -- neighbors(u))
          .foldLeft((sP, sX, largest)) {
            case ((pp, xx, s), v) =>
              (pp - v, xx - v, inner(sR + v, pp.intersect(neighbors(v)), xx.intersect(neighbors(v)), s))
          }
          ._3
      }
    }
    inner(Set(), neighbors.keySet, Set(), Set())
  }

  def distanceToPointWithLargestSet(s: String): Int = {
    val bots = parseBots(s)
    val neighbors: Map[Bot, Set[Bot]] =
      bots.map(b1 => b1 -> bots.filter(b2 => b2 != b1 && b1.inRange(b2)).toSet).toMap
    largestGroup(neighbors).map(b => b.p.mDistance(Point(0, 0, 0, 0)) - b.r).max
  }

  inRangeOfStrongest(Sample1) should be(7)
  inRangeOfStrongest(raw) should be(510)
  distanceToPointWithLargestSet(Sample2) should be(36)
  distanceToPointWithLargestSet(raw) should be(108889300)
}
