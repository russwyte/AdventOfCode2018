package advent.of.code
import fastparse._
import NoWhitespace._
import graph._

import scala.annotation.tailrec

class Day25 extends Day(25) {

  def pPoint[_: P]: P[Point] = P(pInt ~ "," ~ pInt ~ "," ~ pInt ~ "," ~ pInt).map {
    case (x, y, z, t) => Point(x, y, z, t)
  }

  def points(s: String): List[Point] = s.linesIterator.map(l => parse(l.trim, pPoint(_)).get.value).toList

  def inRange(p1: Point, p2: Point): Boolean = (p1 mDistance p2) <= 3

  def graph(ps: List[Point]): GraphMap[Point] = ps.map(a => a -> ps.filter(b => a != b && inRange(a, b))).toMap

  def constellations(s: String): Int = {
    @tailrec
    def inner(graph: GraphMap[Point], acc: Int = 0): Int = {
      if (graph.isEmpty) acc
      else {
        val found = Graph(graph).dfs(graph.head._1)
        inner(graph -- found, acc + 1)
      }
    }
    inner(graph(points(s)))
  }

  val Sample1 = "0,0,0,0\n 3,0,0,0\n 0,3,0,0\n 0,0,3,0\n 0,0,0,3\n 0,0,0,6\n 9,0,0,0\n12,0,0,0"
  val Sample2 = "-1,2,2,0\n0,0,2,-2\n0,0,0,-2\n-1,2,0,0\n-2,-2,-2,2\n3,0,2,-1\n-1,3,2,2\n-1,0,-1,0\n0,2,1,-2\n3,0,0,0"

  constellations(Sample1) should be(2)
  constellations(Sample2) should be(4)
  constellations(raw) should be(390)

}
