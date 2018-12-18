package advent.of.code
import scala.annotation.tailrec

class Day18 extends Day(18) {
  val Sample = """.#.#...|#.
                 |.....#|##|
                 |.|..|...#.
                 |..|#.....#
                 |#.#|||#|#|
                 |...#.||...
                 |.|....|...
                 |||...#|.#|
                 ||.||||..|.
                 |...#.|..|.
                 |""".stripMargin

  type Area = Map[Point, Acre]

  def parse(s: String): Area = {
    s.linesIterator.zipWithIndex.map(line => line._2 -> line._1.zipWithIndex).foldLeft(Map[Point, Acre]()) {
      case (m, (y, xs)) =>
        m ++ xs
          .map(x => {
            val p = Point(x._2, y)
            p -> (x._1 match {
              case '|' => Trees(p)
              case '#' => Yard(p)
              case _   => Open(p)
            })
          })
          .toMap
    }
  }

  def show(a: Area): String = {
    val b      = Bounds(a.keys.toSeq)
    val buffer = new StringBuffer()
    for (y <- b.tl.y to b.br.y) {
      for (x <- b.tl.x to b.br.x) {
        buffer.append(a(Point(x, y)).toString)
      }
      buffer.append('\n')
    }
    buffer.toString
  }

  trait Acre {
    def p: Point
    def neighbors(a: Area): Seq[Acre] = p.compass.toList.flatMap(a.get)
    def adjacentTrees(a: Area): Int   = neighbors(a).collect { case x: Trees => x }.size
    def adjacentOpen(a: Area): Int    = neighbors(a).collect { case x: Open => x }.size
    def adjacentYard(a: Area): Int    = neighbors(a).collect { case x: Yard => x }.size
    def transition(a: Area): Acre
  }
  case class Trees(p: Point) extends Acre {
    override def transition(a: Area): Acre = if (adjacentYard(a) >= 3) Yard(p) else this
    override def toString: String          = "|"
  }
  case class Open(p: Point) extends Acre {
    override def transition(a: Area): Acre = if (adjacentTrees(a) >= 3) Trees(p) else this
    override def toString: String          = "."
  }
  case class Yard(p: Point) extends Acre {
    override def transition(a: Area): Acre = if (adjacentYard(a) >= 1 && adjacentTrees(a) >= 1) this else Open(p)
    override def toString: String          = "#"
  }

  def value(s: String, n: Int): Int = {
    val area = parse(s)
    val b    = Bounds(area.keys)
    @tailrec
    def inner(a: Area, n: Int): Area = {
//      println(show(a))
      if (n == 0) a
      else {
        inner(tick(a, b), n - 1)
      }
    }
    val na = inner(area, n)
    calc(na)
  }

  def calc(a: Area): Int = {
    a.values.collect { case x: Yard => x }.size * a.values.collect { case x: Trees => x }.size
  }

  def findFirstRepeated(s: String): (Int, List[Int]) = {
    val area = parse(s)
    val b    = Bounds(area.keys)
    @tailrec
    def inner(a: Area, n: Int, as: List[Area]): (Int, List[Int]) = {
      val aa = tick(a, b)
      if (as.contains(aa)) {
        val res = as.reverse
        val x   = res.indexOf(aa)
        println(x, res.size)
        (x, res.map(calc))
      } else inner(aa, n + 1, aa :: as)
    }
    inner(area, 0, List(area))
  }

  def part2(s: String, n: Long): Int = {
    val (p, as) = findFirstRepeated(s)
    val size    = as.length - p
    if (n < p) as(n.toInt) else as((p + ((n - p) % size)).toInt)
  }

  def tick(area: Area, b: Bounds): Area = {
    b.boundedPoints.foldLeft(area) { case (na, p) => na.updated(p, area(p).transition(area)) }
  }

  "part 1 and 2" should "work" in {
    value(Sample, 10) should be(1147)
    value(raw, 10) should be(505895)
    part2(raw, 1000000000) should be(139590)
  }
}
