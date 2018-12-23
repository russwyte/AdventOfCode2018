package advent.of.code

case class Bounds(tl: Point, br: Point) {
  def boundedPoints: Seq[Point]   = xRange.flatMap(x => yRange.map(y => Point(x, y)))
  def xRange: Range               = tl.x to br.x
  def yRange: Range               = tl.y to br.y
  def contains(p: Point): Boolean = tl.x <= p.x && tl.y <= p.y && br.x >= p.x && br.y >= p.y
  def width: Int                  = br.x - tl.x
  def height: Int                 = br.y - tl.y
  def area: Int                   = width * height
}

object Bounds {
  def apply(ps: Iterable[Point]): Bounds = {
    val tl = Point(ps.minBy(_.x).x, ps.minBy(_.y).y)
    val br = Point(ps.maxBy(_.x).x, ps.maxBy(_.y).y)
    Bounds(tl, br)
  }
}
