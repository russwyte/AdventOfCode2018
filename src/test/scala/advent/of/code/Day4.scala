package advent.of.code

import fastparse.NoWhitespace._
import fastparse._

class Day4 extends Day(4) {

  val Sample =
    "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up".linesIterator.toList

  def pStartShift[_: P]: P[StartShift] =
    P(
      pDateWithTime ~
        " Guard #" ~
        pUnsignedInt ~
        " begins shift").map {
      StartShift.tupled
    }

  def pSleep[_: P]: P[Sleep] = P(pDateWithTime ~ " falls asleep").map(Sleep)

  def pWake[_: P]: P[Wake] = P(pDateWithTime ~ " wakes up").map(Wake)

  def pEvent[_: P]: P[Event] = P(pStartShift | pSleep | pWake)

  sealed trait Event {
    def time: DateWithTime

    def isStart: Boolean = false
  }

  case class StartShift(time: DateWithTime, id: Int) extends Event {
    override def isStart: Boolean = true
  }

  case class Sleep(time: DateWithTime) extends Event

  case class Wake(time: DateWithTime) extends Event

  case class Shift(id: Int, sleptMiniutes: Seq[Int] = Seq.empty) {
    def apply(sleep: Sleep, wake: Wake): Shift = {
      val m = sleptMiniutes ++ (sleep.time.minute until wake.time.minute)
      copy(sleptMiniutes = m)
    }
  }

  def events(ls: List[String]): List[Event] = ls.map(parse(_, pEvent(_)).get.value).sortBy(_.time)

  def shifts(events: List[Event]): List[Shift] = {
    def inner(ls: List[Event], acc: List[Shift]): List[Shift] = {
      ls match {
        case (s: StartShift) :: xs =>
          val start = Shift(s.id)
          val p     = xs.span(!_.isStart)
          val shift = p._1.takeWhile(!_.isStart).sliding(2, 2).foldLeft(start) {
            case (x, es) =>
              es match {
                case List(s: Sleep, w: Wake) => x.apply(s, w)
                case _                       => x
              }
          }
          inner(p._2, shift :: acc)
        case _ => acc
      }
    }

    inner(events, Nil)
  }

  def sleepy(shifts: List[Shift]): Int =
    shifts.groupBy(_.id).toList.maxBy(_._2.foldLeft(0)((a, x) => a + x.sleptMiniutes.length))._1

  def part1(shifts: List[Shift]) = {
    val id  = sleepy(shifts)
    val ss  = shifts.filter(_.id == id).map(_.sleptMiniutes.sorted)
    val min = ss.foldLeft(60)((acc, x) => if (x.head < acc) x.head else acc)
    val max = ss.foldLeft(0)((acc, x) => if (x.last > acc) x.last else acc)

    def occurs(n: Int): Int = ss.count(_.contains(n))

    id * (min to max).map(n => n -> occurs(n)).maxBy(_._2)._1
  }

  def part2(shifts: List[Shift]): Int = {

    def mostSleptMinute(ls: List[Shift]): (Int, Int) =
      (0 until 60).map(n => (n, ls.count(_.sleptMiniutes.contains(n)))).maxBy(_._2)

    val ss  = shifts.groupBy(_.id).map(x => x._1 -> mostSleptMinute(x._2))
    val res = ss.maxBy(_._2._2)
    res._1 * res._2._1
  }

  "Day4" should "work" in {
    part1(shifts(events(Sample))) should be(240)
    part1(shifts(events(lines))) should be(143415)
    part2(shifts(events(Sample))) should be(4455)
    part2(shifts(events(lines))) should be(49944)
  }
}
