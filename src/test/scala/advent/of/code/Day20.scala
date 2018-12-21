package advent.of.code
import fastparse._
import NoWhitespace._

import scala.collection.mutable

class Day20 extends Day(20) {

  type Doors = mutable.Map[Point, Set[Point]]

  sealed trait Regex {
    def strings: Seq[String]
    def allDoors(doors: Doors, points: Set[Point] = Set(Point(0, 0))): Set[Point]
  }

  case class Node(s: String) extends Regex {
    override def strings: Seq[String] = Seq(s)
    override def allDoors(doors: Doors, points: Set[Point]): Set[Point] = {
      val pMoves = points.map(p => move(s.toList, p))
      for {
        moves    <- pMoves
        (p1, p2) <- moves.zip(moves.tail)
      } {
        doors(p1) += p2
        doors(p2) += p1
      }

      pMoves.map(m => m.last)
    }
  }

  case class Choice(nodes: Seq[Regex]) extends Regex {
    override def strings: Seq[String] = nodes.flatMap(_.strings)
    override def allDoors(doors: Doors, poss: Set[Point]): Set[Point] = {
      nodes.flatMap(r => r.allDoors(doors, poss)).toSet
    }
  }

  case class Concat(nodes: Seq[Regex]) extends Regex {
    override def strings: Seq[String] =
      nodes.foldLeft(Seq(""))((s, r) => {
        for {
          s1 <- s
          s2 <- r.strings
        } yield {
          s1 + s2
        }
      })
    override def allDoors(doors: Doors, poss: Set[Point]): Set[Point] = {
      nodes.foldLeft(poss)({ (acc, node) =>
        node.allDoors(doors, acc)
      })
    }
  }

  val MoveMap: Map[Char, Point => Point] = Map(
    'N' -> (_.north),
    'S' -> (_.south),
    'E' -> (_.east),
    'W' -> (_.west)
  )

  def move(cs: List[Char], point: Point): List[Point] = cs match {
    case Nil     => List(point)
    case x :: xs => point :: move(xs, MoveMap(x)(point))
  }

  def find(doors: Map[Point, Set[Point]], pos: Point = Point(0, 0)): Map[Point, Int] = {

    def inner(visited: Map[Point, Int], toVisit: Map[Point, Int]): Map[Point, Int] = {
      val neighbors = for {
        (vp, vd) <- toVisit
        p        <- doors(vp)
      } yield p -> (vd + 1)
      val vv = visited ++ toVisit
      val tv = neighbors.filterKeys(!visited.contains(_))
      if (tv.isEmpty) vv else inner(vv, tv)
    }

    inner(Map.empty, Map(pos -> 0))
  }

  def roomDistances(regex: Regex): Map[Point, Int] = {
    val doors: Doors = mutable.Map.empty.withDefaultValue(Set.empty)
    regex.allDoors(doors)
    find(doors.toMap)
  }

  def farthest(input: String): Int = {
    roomDistances(parse(input, pAll(_)).get.value).values.max
  }

  def count(input: String, threshold: Int): Int = {
    roomDistances(parse(input, pAll(_)).get.value).values.count(_ >= threshold)
  }

  def pStringNode[_: P]: P[Node] = P(CharIn("NSEW").rep(1).!).map(Node)

  def pEmpty[_: P]: P[Node] = P("").map(_ => Node(""))

  def pConcatNode[_: P]: P[Concat] = P(pRegexNode.rep(1)).map(Concat)

  def pRegexNode[_: P]: P[Regex] = P(pStringNode | ("(" ~ (pConcatNode | pEmpty).rep(1, "|") ~ ")").map(Choice))

  def pAll[_: P]: P[Concat] = P("^".? ~ pConcatNode ~ "$".?)

  case class Route(start: Node, options: Seq[Route])

  farthest(raw) should be(3415)
  count(raw, 1000) should be(8583)
}
