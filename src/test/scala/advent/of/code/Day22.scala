package advent.of.code
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.mutable

import Dijkstra._

class Day22 extends FlatSpec with Matchers {
  case class System(depth: Int, target: Point) {
    private val erosionLevels       = mutable.Map[Point, Int]()
    private val regions             = mutable.Map[Point, Region]()
    def erosionLevel(p: Point): Int = erosionLevels.getOrElseUpdate(p, (depth + geologicIndex(p)) % 20183)
    def geologicIndex(p: Point): Int = {
      if (p == Point(0, 0) || p == target) 0
      else if (p.y == 0) 16807 * p.x
      else if (p.x == 0) 48271 * p.y
      else
        erosionLevels.getOrElseUpdate(p.west, erosionLevel(p.west)) * erosionLevels.getOrElseUpdate(
          p.north,
          erosionLevel(p.north))
    }
    def apply(p: Point): Region = {
      regions.getOrElseUpdate(p, Region(erosionLevel(p)))
    }
    def risk: Int = Bounds(Point(0, 0), target).boundedPoints.map(apply).map(_.risk).sum
  }
  sealed trait Region {
    def risk: Int
    def char: Char
    def tools: Set[Tool]
    def nextTool(that: Region, tool: Tool): Tool = {
      if (this == that) tool else if (tools(tool)) tool else tools.intersect(that.tools).head
    }
  }
  object Region {
    def apply(errosionInddex: Int): Region = errosionInddex % 3 match {
      case 0 => Rocky
      case 1 => Wet
      case _ => Narrow
    }
  }
  case object Rocky extends Region {
    val risk             = 0
    val char             = '.'
    val tools: Set[Tool] = Set(ClimbingGear, Torch)
  }

  case object Wet extends Region {
    val risk             = 1
    val char             = '='
    val tools: Set[Tool] = Set(ClimbingGear, Neither)
  }
  case object Narrow extends Region {
    val risk             = 2
    val char             = '|'
    val tools: Set[Tool] = Set(Torch, Neither)
  }

  def part1(depth: Int, target: Point): Int = System(depth, target).risk

  sealed trait Tool
  case object Torch        extends Tool
  case object ClimbingGear extends Tool
  case object Neither      extends Tool

  def part2(depth: Int, target: Point): Int = {
    val system = System(depth, target)
    val bounds = Bounds(Point(0, 0), Point(target.x * 9, (target.y * 1) + 10))

    case class State(point: Point, tool: Tool)

    def cost(state: State, p: Point): (State, Int) = {
      val region = system(state.point)
      val dest   = system(p)
      val tool   = if (p == target) Torch else dest.nextTool(region, state.tool)
      State(p, tool) -> (if (tool == state.tool) 1 else 8)
    }

    val graph: Graph[State] = s => {
      val region = system(s.point)
      val points = s.point.cardinal.filter(p => bounds.contains(p))
      val res: Map[State, Int] = points.map { p =>
        cost(s, p)
      }.toMap
      res
    }
    val res = dijkstra(graph)(State(Point(0, 0), Torch))
    res._1(State(target, Torch))
  }

  part1(510, Point(10, 10)) should be(114)
  part2(510, Point(10, 10)) should be(45)
  part1(4845, Point(6, 770)) should be(5400)
  part2(4845, Point(6, 770)) should be(1048)

}
