package advent.of.code

case class Bounds(tl: Point, br: Point) {
  def boundedPoints: Seq[Point] = {
    (tl.x to br.x).flatMap(x => (tl.y to br.y).map(y => Point(x, y)))
  }

  def contains(p: Point): Boolean = tl.x <= p.x && tl.y <= p.y && br.x >= p.x && br.y >= p.y
  def width                       = br.x - tl.x
  def height: Int                 = br.y - tl.y
  def area                        = width * height
}

object Bounds {
  def apply(ps: Seq[Point]): Bounds = {
    val tl = Point(ps.minBy(_.x).x, ps.minBy(_.y).y)
    val br = Point(ps.maxBy(_.x).x, ps.maxBy(_.y).y)
    Bounds(tl, br)
  }
}
