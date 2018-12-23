package advent.of.code

case class Point(x: Int, y: Int, z: Int) {
  def mDistance(p: Point): Int  = Math.abs(x - p.x) + Math.abs(y - p.y) + Math.abs(z - p.z)
  def unary_-(): Point          = Point(-x, -y, -z)
  def +(other: Point)           = Point(x + other.x, y + other.y, z + other.z)
  def north: Point              = copy(y = y - 1)
  def south: Point              = copy(y = y + 1)
  def east: Point               = copy(x = x + 1)
  def west: Point               = copy(x = x - 1)
  def northeast: Point          = north.east
  def northwest: Point          = north.west
  def southwest: Point          = south.west
  def southeast: Point          = south.east
  def cardinal: Set[Point]      = Set(north, south, east, west)
  def interCardinal: Set[Point] = Set(northeast, northwest, southeast, southwest)
  def compass: Set[Point]       = cardinal ++ interCardinal
}
object Point {
  // construct a 2D point
  def apply(x: Int, y: Int) = new Point(x, y, 0)
}
