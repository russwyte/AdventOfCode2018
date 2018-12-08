package advent.of.code
import fastparse.NoWhitespace._
import fastparse._

class Day8 extends Day(8) {
  case class Node(children: Seq[Node], metaData: Seq[Int]) {
    def sum: Int = metaData.sum + children.map(_.sum).sum
    def value: Int = {
      if (children.isEmpty) metaData.sum
      else {
        metaData.map(n => children.lift(n - 1).map(_.value).getOrElse(0)).sum
      }
    }
  }
  case class NodeHeader(childCount: Int, metaDataCount: Int)

  def pMetaData[_: P](n: Int): P[Seq[Int]] = P(pUnsignedInt.rep(n, " ", n))

  def pChildren[_: P](n: Int): P[Seq[Node]] = {
    if (n == 0) {
      Pass.map(_ => Seq.empty)
    } else {
      " " ~ pNode.rep(n, " ", n)
    }
  }

  def pNodeBody[_: P](h: NodeHeader): P[Node] =
    P(pChildren(h.childCount) ~ " " ~ pMetaData(h.metaDataCount)).map {
      case (c, m) => Node(c, m)
    }

  def pNodeHeader[_: P]: P[NodeHeader] = P(pUnsignedInt ~ " " ~ pUnsignedInt).map(NodeHeader.tupled)

  def pNode[_: P]: P[Node]  = P(pNodeHeader).flatMap(nh => pNodeBody(nh))
  val Sample                = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  def node(s: String): Node = parse(s, pNode(_)).get.value
  "part1" should "succeed" in {
    node(Sample).sum should be(138)
    node(raw).sum should be(46829)
    node(Sample).value should be(66)
    node(raw).value should be(37450)
  }
}
