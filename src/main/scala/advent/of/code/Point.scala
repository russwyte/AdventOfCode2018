package advent.of.code

case class Point(x: Int, y: Int) {
  def mDistance(p: Point): Int     = Math.abs(x - p.x) + Math.abs(y - p.y)
  def unary_-(): Point             = Point(-x, -y)
  def +(other: Point)              = Point(x + other.x, y + other.y)
  def north                        = copy(y = y - 1)
  def south                        = copy(y = y + 1)
  def east                         = copy(x = x + 1)
  def west                         = copy(x = x - 1)
  def directlyAdjacent: Set[Point] = Set(north, south, east, west)
}
