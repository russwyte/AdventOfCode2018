package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

object Day1 extends Day {

  override def dayNumber: Int = 1

  object Part1 {
    type Operation = (BigInt, BigInt) => BigInt
    val add: Operation = (x, y) => x + y
    val subtract: Operation = (x, y) => x - y
    val operations: Map[Char, Operation] = Map('+' -> add, '-' -> subtract)

    def apply(s: String*): BigInt = {
      s.foldLeft(BigInt(0)) { (x, line) =>
        operations(line.head)(x, BigInt(line.tail))
      }
    }
  }

  object Part2 {

    import Part1._

    case class History(n: BigInt, counts: Map[BigInt, BigInt]) {
      def apply(n: BigInt) =
        copy(n, counts.updated(n, counts.getOrElse(n, BigInt(0)) + 1))
    }

    object History {
      def apply(n: BigInt) = new History(n, Map(n -> BigInt(1)))
    }

    def apply(s: String*): History = {
      def inner(h: History, ls: List[String]): History = {
        ls match {
          case Nil => inner(h, s.toList)
          case x :: xs =>
            val nh = h(operations(x.head)(h.n, BigInt(x.tail)))
            if (nh.counts(nh.n) == BigInt(2)) {
              nh
            } else {
              inner(nh, xs)
            }
        }
      }

      inner(History(0), s.toList)
    }
  }

}

class Day1Specs extends FlatSpec with Matchers {

  import advent.of.code.Day1.Part1


  "applying +1,-1,+1" should "be 1" in {
    Part1("+1", "-1", "+1") should be(1)
  }

  "applying the input" should "work as expected" in {
    Part1(Day1.input: _*) should be(543)
  }

  "Part2" should "work" in {
    import advent.of.code.Day1.Part2
    Part2("+1", "-1", "+3").n should be(0)
    Part2("+1", "-2", "+3", "+1", "+1", "-2", "+100").n should be(2)
    Part2(Day1.input: _*).n should be(621)
  }
}
