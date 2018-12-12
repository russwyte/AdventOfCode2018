package advent.of.code
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class Day12 extends FlatSpec with Matchers {
  type Pots  = Map[Long, Pot]
  type Rules = Map[String, Char]
  val InputState: Pots =
    pots("###.#..#..##.##.###.#.....#.#.###.#.####....#.##..#.#.#..#....##..#.##...#.###.#.#..#..####.#.##.#")
  val InputRules: Rules =
    rules(
      "#.... => .\n#.##. => #\n..#.. => .\n#.#.# => .\n.#.## => #\n...## => #\n##... => #\n###.. => #\n#..## => .\n.###. => .\n###.# => #\n..... => .\n#..#. => .\n.#.#. => #\n##..# => #\n.##.. => .\n...#. => .\n#.### => .\n..### => .\n####. => .\n#.#.. => #\n.##.# => #\n.#... => #\n##.#. => #\n....# => .\n..#.# => #\n#...# => #\n..##. => .\n.#..# => #\n.#### => .\n##### => #\n##.## => #")
  val Sample: Pots = pots("#..#.#..##......###...###")
  val SampleRules: Rules =
    rules(
      "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #")

  case class Pot(number: Long, char: Char) {
    def hasPlant: Boolean = char == '#'
  }

  def generation(pots: Pots, r: Rules): Pots = {
    val l = for {
      n <- (pots.minBy(_._1)._1 - 3) to pots.maxBy(_._1)._1 + 3
    } yield {
      val c = r(context(pots, n))
      (n, Pot(n, c))
    }
    val tl = l.splitAt(l.lastIndexWhere(_._2.hasPlant) + 1)._1.dropWhile(_._2.char == '.')
    tl.toMap.withDefault(n => Pot(n, '.'))
  }

  def showPots(pots: Pots): String = pots.values.toList.sortBy(_.number).foldLeft("")(_ + _.char)

  def pots(s: String): Pots =
    s.zipWithIndex.map { case (c, n) => n.toLong -> Pot(n, c) }.toMap.withDefault(n => Pot(n, '.'))

  def context(pots: Pots, n: Long): String =
    "" + pots(n - 2).char + pots(n - 1).char + pots(n).char + pots(n + 1).char + pots(n + 2).char

  def potNumberSum(pots: Pots): Long = pots.values.foldLeft(0L)((s, p) => s + (if (p.hasPlant) p.number else 0))

  def rules(s: String): Rules =
    s.linesIterator.map(line => line.substring(0, 5) -> line.charAt(9)).toMap.withDefault(_ => '.')

  def potSum(pots: Pots, r: Rules, generations: Long): Long = {
    @tailrec
    def inner(count: Long, p: Pots): Pots = {
      val p2 = generation(p, r)
      if (count < 1) p
      else if (showPots(p) == showPots(p2)) {
        p.mapValues(p => p.copy(number = p.number + count))
      } else {
        inner(count - 1, p2)
      }
    }
    potNumberSum(inner(generations, pots))
  }

  "plant state" should "be correct" in {
    context(Sample, -3) should be(".....")
    context(Sample, 0) should be("..#..")
    context(Sample, 1) should be(".#..#")
    context(Sample, 2) should be("#..#.")
    context(Sample, Sample.size) should be("##...")
    context(Sample, Sample.size + 2) should be(".....")
  }
  "part1" should "work" in {
    potSum(Sample, SampleRules, 20) should be(325)
    potSum(InputState, InputRules, 20) should be(2909)
  }
  "part2" should "work" in {
    potSum(Sample, SampleRules, 200L) should be(3374)
    potSum(InputState, InputRules, 50000000000L) should be(2500000001175L)
  }

}
