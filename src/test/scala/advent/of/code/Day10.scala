package advent.of.code
import fastparse.NoWhitespace._
import fastparse._

import scala.annotation.tailrec

class Day10 extends Day(10) {
  val Sample =
    "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>".linesIterator
      .map(parse(_, pStar(_)).get.value)
      .toSeq
  val Input = lines.map(parse(_, pStar(_)).get.value)

  case class Star(position: Point, velocity: Point) {
    def advance: Star = copy(position = Point(position.x + velocity.x, position.y + velocity.y))
  }

  def pN[_: P]: P[Int]       = P(" ".? ~ pInt)
  def pPoint[_: P]: P[Point] = P("<" ~ pN ~ ", " ~ pN ~ ">").map(Point.tupled)
  def pStar[_: P]: P[Star]   = P("position=" ~ pPoint ~ " velocity=" ~ pPoint).map(Star.tupled)

  def bounds(stars: Seq[Star]) = Bounds(stars.map(_.position))

  case class Result(message: String, elapsed: Int)
  @tailrec
  final def find(stars: Seq[Star], n: Int = 0): Result = {
    val b    = bounds(stars)
    val next = stars.map(_.advance)
    // if any star in the next set escapes the bounds we know we are done
    if (next.exists(x => !b.contains(x.position))) Result(read(stars), n) else find(next, n + 1)
  }
  def read(stars: Seq[Star]): String = {
    val m      = stars.map(_.position).toSet
    val b      = bounds(stars)
    val buffer = new StringBuffer()
    for {
      y <- b.tl.y to b.br.y
    } {
      for {
        x <- b.tl.x to b.br.x
      } {
        val p = Point(x, y)
        if (m(p)) buffer.append('#') else buffer.append('.')
      }
      buffer.append('\n')
    }
    buffer.toString
  }
  "finding" should "work" in {
    val r1 = find(Sample)
    r1.elapsed should be(3)
    r1.message should be(
      """#...#..###
        |#...#...#.
        |#...#...#.
        |#####...#.
        |#...#...#.
        |#...#...#.
        |#...#...#.
        |#...#..###
        |""".stripMargin
    )
    val r2 = find(Input)
    r2.elapsed should be(10418)
    r2.message should be(
      """######..#....#..#....#..#####...######.....###..#....#..#####.
        |.....#..##...#..##...#..#....#.......#......#...#....#..#....#
        |.....#..##...#..##...#..#....#.......#......#....#..#...#....#
        |....#...#.#..#..#.#..#..#....#......#.......#....#..#...#....#
        |...#....#.#..#..#.#..#..#####......#........#.....##....#####.
        |..#.....#..#.#..#..#.#..#..#......#.........#.....##....#.....
        |.#......#..#.#..#..#.#..#...#....#..........#....#..#...#.....
        |#.......#...##..#...##..#...#...#.......#...#....#..#...#.....
        |#.......#...##..#...##..#....#..#.......#...#...#....#..#.....
        |######..#....#..#....#..#....#..######...###....#....#..#.....
        |""".stripMargin
    )
  }
}
