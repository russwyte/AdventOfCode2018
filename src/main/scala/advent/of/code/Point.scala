package advent.of.code

case class Point(x: Int, y: Int) {
  def mDistance(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)
  def unary_-(): Point         = Point(-x, -y)
  def +(other: Point)          = Point(x + other.x, y + other.y)
  def north: Point             = copy(y = y - 1)
  def south: Point             = copy(y = y + 1)
  def east: Point              = copy(x = x + 1)
  def west: Point              = copy(x = x - 1)
  def northeast: Point         = north.east
  def northwest: Point         = north.west
  def southwest: Point         = south.west
  def southeast: Point         = south.east

  def cardinal: Set[Point]      = Set(north, south, east, west)
  def interCardinal: Set[Point] = Set(northeast, northwest, southeast, southwest)
  def compass: Set[Point]       = cardinal ++ interCardinal
}
