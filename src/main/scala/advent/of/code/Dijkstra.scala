package advent.of.code
import scala.annotation.tailrec

object Dijkstra {
  def dijkstra[N](g: Graph[N])(source: N): (Map[N, Int], Map[N, N]) = {
    @tailrec
    def go(active: Set[N], res: Map[N, Int], pred: Map[N, N]): (Map[N, Int], Map[N, N]) =
      if (active.isEmpty) (res, pred)
      else {
        val node = active.minBy(res)
        val cost = res(node)
        val neighbours = for {
          (n, c) <- g(node) if cost + c < res.getOrElse(n, Int.MaxValue)
        } yield n -> (cost + c)
        val active1 = active - node ++ neighbours.keys
        val preds   = neighbours mapValues (_ => node)
        go(active1, res ++ neighbours, pred ++ preds)
      }

    go(Set(source), Map(source -> 0), Map.empty)
  }

  def shortestPath[N](g: Graph[N])(source: N, target: N): Option[List[N]] = {
    val pred = dijkstra(g)(source)._2
    if (pred.contains(target) || source == target)
      Some(iterateRight(target)(pred.get))
    else None
  }

  def iterateRight[N](x: N)(f: N => Option[N]): List[N] = {
    def go(x: N, acc: List[N]): List[N] = f(x) match {
      case None    => x :: acc
      case Some(y) => go(y, x :: acc)
    }

    go(x, List.empty)
  }

}
