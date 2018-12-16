package advent.of.code

trait Graph[T] {

  type GraphMap = Map[T, List[T]]

  def graphMap: GraphMap

  def breadthFirstSearch(start: T): List[List[T]] = {

    def inner(elems: List[T], visited: List[List[T]]): List[List[T]] = {
      val newNeighbors = elems.flatMap(graphMap(_)).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        visited
      else
        inner(newNeighbors, newNeighbors :: visited)
    }

    inner(List(start), List(List(start))).reverse
  }

  def depthFirstSearch(start: T): List[T] = {

    def inner(v: T, visited: List[T]): List[T] = {
      if (visited.contains(v))
        visited
      else {
        val neighbours: List[T] = graphMap(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b, a) => inner(a, b))
      }
    }
    inner(start, List()).reverse
  }
}
