package advent.of.code

import fastparse._
import NoWhitespace._

class Day17 extends Day(17) {
  val Sample  = """x=495, y=2..7
                 |y=7, x=495..501
                 |x=501, y=3..7
                 |x=498, y=2..4
                 |x=506, y=1..2
                 |x=498, y=10..13
                 |x=504, y=10..13
                 |y=13, x=498..504""".stripMargin
  val Flowing = '|'
  val Settled = '~'
  val Clay    = '#'
  val Sand    = '.'
  type Soil = Map[Point, Char]
  def pXorY[_: P]: P[String]              = P(("x" | "y").! ~ "=")
  def pSingle[_: P]: P[(String, Int)]     = P(pXorY ~ pUnsignedInt)
  def pRange[_: P]: P[(String, Int, Int)] = P(pXorY ~ pUnsignedInt ~ ".." ~ pUnsignedInt)
  def pEntry[_: P]: P[Seq[Point]] = P(pSingle ~ ", " ~ pRange).map {
    case ("x", x, ("y", y1, y2)) => (y1 to y2).map(y => Point(x, y))
    case (_, y, (_, x1, x2))     => (x1 to x2).map(x => Point(x, y))
  }
  def soil(s: String): Soil = {
    s.linesIterator
      .map(parse(_, pEntry(_)).get.value)
      .flatMap(ps => ps.map(_ -> Clay))
      .toMap
      .withDefaultValue(Sand)
  }

  def show(soil: Soil): String = {
    val buffer = new StringBuffer()
    val b      = Bounds(soil.keys.toSeq)
    for {
      y <- b.tl.y to b.br.y
    } {
      for {
        x <- b.tl.x to b.br.x
      } {
        buffer.append(soil(Point(x, y)))
      }
      buffer.append("\n")
    }
    buffer.toString
  }

  case class Flood(soil: Soil, bounds: Bounds) {
    def flood(p: Point): Char = {
      soil(p) match {
        case Flowing =>
          if (shouldSettle(p)) Settled else Flowing
        case Sand => Flowing
        case x    => x
      }
    }
    def leftClay(p: Point): Option[Point] = {
      if (bounds.contains(p)) {
        if (soil(p) == Clay) {
          Some(p)
        } else {
          leftClay(p.west)
        }
      } else {
        None
      }
    }
    def rightClay(p: Point): Option[Point] = {
      if (bounds.contains(p)) {
        if (soil(p) == Clay) {
          Some(p)
        } else {
          rightClay(p.east)
        }
      } else {
        None
      }
    }

    def spreadRight(p: Point): Int = {
      val b = soil(p.south)
      if ((b == Clay || b == Settled) && soil(p.east) == Sand) spreadRight(p.east) else p.x
    }
    def spreadLeft(p: Point): Int = {
      val b = soil(p.south)
      if ((b == Clay || b == Settled) && soil(p.west) == Sand) spreadLeft(p.west) else p.x
    }

    def shouldSettle(p: Point): Boolean = {
      soil(p) match {
        case Flowing =>
          leftClay(p)
            .flatMap(l =>
              rightClay(p).map(r => {
                (l.x to r.x).forall(x => {
                  val c = soil(Point(x, p.y))
                  c == Clay || c == Flowing
                })
              }))
            .getOrElse(false)
        case _ => false
      }
    }
    def flood: Flood = {
      val ss = soil.filter(_._2 == Flowing).foldLeft(soil) {
        case (s, (p, c)) =>
          val under = s(p.south)
          under match {
            case Clay | Settled =>
              val left  = (spreadLeft(p) to p.x).map(x => Point(x, p.y)).map(p => p -> flood(p))
              val right = (p.x to spreadRight(p)).map(x => Point(x, p.y)).map(p => p -> flood(p))
              val mm    = left ++ right
              mm.filter(x => bounds.br.y >= x._1.y)
              s ++ mm
            case _ => if (bounds.br.y >= p.south.y) s.updated(p.south, Flowing) else s
          }
      }
      copy(soil = ss)
    }
    def count(c: Char) = soil.count { case (_, sc) => sc == c }
    def countWet: Int  = count(Settled) + count(Flowing)
  }
  object Flood {
    def apply(soil: Soil): Flood = {
      val b = Bounds(soil.keys.toSeq)
      new Flood(soil.updated(Point(500, b.tl.y), Flowing), b)
    }
  }

  val s = soil(Sample)
  def part1(s: String): Int = {
    def inner(flood: Flood, last: Soil = Map.empty, count: Int = 0): Flood = {
      if (flood.soil == last) {
        flood
      } else inner(flood.flood, flood.soil, count + 1)
    }
    inner(Flood(soil(s))).countWet
  }
  def part2(s: String): Int = {
    def inner(flood: Flood, last: Soil = Map.empty, count: Int = 0): Flood = {
      if (flood.soil == last) {
        flood
      } else inner(flood.flood, flood.soil, count + 1)
    }
    inner(Flood(soil(s))).count(Settled)
  }

  "things" should "work" in {
    part1(Sample) should be(57)
    part2(Sample) should be(29)
    //part1(raw) should be(27206)
    //part2(raw) should be(21787)
  }

}
