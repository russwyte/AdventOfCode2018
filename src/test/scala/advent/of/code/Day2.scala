package advent.of.code

import org.scalatest.{FlatSpec, Matchers}

object Day2 extends Day {

  override def dayNumber: Int = 2

  object Part1 {

    case class ID(s: String) {
      private val occurrences = s.groupBy(identity).map(_._2.length)

      def occurs(n: Int): Boolean = occurrences.count(_ == n) >= 1

      def check: Boolean = occurs(2) && occurs(3)
    }

    def checkSum(ss: Seq[String]): Int = {
      val ids = ss.map(ID)
      ids.count(_.occurs(2)) * ids.count(_.occurs(3))
    }
  }

  object Part2 {
    def find(ss: Seq[String]): Seq[String] = {
      val length = ss.head.length
      ss.combinations(2)
        .map { p =>
          p.head
            .zip(p.last)
            .collect {
              case (a, b) if a == b => a
            }
            .mkString
        }
        .filter(length - _.length == 1)
    }.toSeq
  }

}

class Day2Specs extends FlatSpec with Matchers {

  import Day2.Part1._
  import Day2.Part2._


  val ss = Seq("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
  "has chars that occur twice" should "work" in {
    ss.map(ID(_).occurs(2)) should be(
      Seq(false, true, true, false, true, true, false)
    )
  }
  "has chars that occur thrice" should "work" in {
    ss.map(ID(_).occurs(3)) should be(
      Seq(false, true, false, true, false, false, true)
    )
  }
  "check for part1" should "work" in {
    ss.map(ID(_).check) should be(
      Seq(
        false, true, false, false, false, false, false
      ))
  }
  "checksum for part1" should "work" in {
    checkSum(ss) should be(12)
    checkSum(Day2.input) should be(7350)
  }
  "finding for part2" should "work" in {
    find(Seq("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")) should be(Seq("fgij"))
    find(Day2.input) should be(Seq("wmlnjevbfodamyiqpucrhsukg"))
  }
}
