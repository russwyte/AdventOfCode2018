package advent.of.code

case class Point(x: Int, y: Int) {
  def mDistance(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)
  def unary_-(): Point         = Point(-x, -y)
  def +(other: Point)          = Point(x + other.x, y + other.y)
}
