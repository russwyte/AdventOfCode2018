package advent.of.code

class Day6 extends Day(6) {

  val points: List[Point] = lines.map { s =>
    val Array(x, y) = s.split(", ").map(_.toInt)
    Point(x, y)
  }

  case class Point(x: Int, y: Int) {
    def mDistance(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)
  }

  case class Bounds(tl: Point, br: Point) {
    def boundedPoints: Seq[Point] = {
      (tl.x to br.x).flatMap(x => (tl.y to br.y).map(y => Point(x, y)))
    }

    def contains(p: Point): Boolean = tl.x < p.x && tl.y < p.y && br.x > p.x && br.y > p.y
  }

  object Bounds {
    def apply(ps: Seq[Point]): Bounds = {
      val tl = Point(ps.minBy(_.x).x, ps.minBy(_.y).y)
      val br = Point(ps.maxBy(_.x).x, ps.maxBy(_.y).y)
      Bounds(tl, br)
    }
  }

  def part1(ps: Seq[Point]): Int = {
    def closest(location: Point): Option[Point] = {
      ps.map(p => p -> p.mDistance(location)).sortBy { case (_, d) => d }.toList match {
        case (pa, da) :: (_, db) :: _ if da < db => Some(pa)
        case _                                   => None
      }
    }

    val bounds                  = Bounds(ps)
    val grid: Map[Point, Point] = bounds.boundedPoints.flatMap(bp => closest(bp).map(cp => bp -> cp)).toMap
    val finite                  = grid.filterKeys(bounds.contains)
    finite.groupBy(_._2).mapValues(_.size).values.max
  }

  def part2(ps: Seq[Point], safe: Int): Int = Bounds(ps).boundedPoints.count(x => ps.map(_.mDistance(x)).sum < safe)

  val sample =
    List(Point(1, 1), Point(1, 6), Point(8, 3), Point(3, 4), Point(5, 5), Point(8, 9)).sortBy(a => (a.x, a.y))

  "Day6" should "go without a hitch" in {
    part1(sample) should be(17)
    part1(points) should be(3238)
    part2(sample, 32) should be(16)
    part2(points, 10000) should be(45046)
  }
}
