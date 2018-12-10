package advent.of.code

case class Point(x: Int, y: Int) {
  def mDistance(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)
  def unary_-(): Point         = Point(-x, -y)
}
