package advent.of.code.graph

case class Graph[Vertex](g: GraphMap[Vertex]) {

  def bfs(start: Vertex): List[List[Vertex]] = {

    def inner(elems: List[Vertex], visited: List[List[Vertex]]): List[List[Vertex]] = {
      val newNeighbors = elems.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        visited
      else
        inner(newNeighbors, newNeighbors :: visited)
    }

    inner(List(start), List(List(start))).reverse
  }

  def dfs(start: Vertex): List[Vertex] = {

    def inner(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v))
        visited
      else {
        val neighbours: List[Vertex] = g(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b, a) => inner(a, b))
      }
    }
    inner(start, List()).reverse
  }
}
