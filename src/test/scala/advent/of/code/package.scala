package advent.of

import fastparse.NoWhitespace._
import fastparse._

package object code {
  def pUnsignedInt[_: P]: P[Int] = P(CharPred(c => '0' <= c && c <= '9').rep(1).!).map(_.toInt)

  // [1518-05-12 00:39] wakes up
  def pDateWithTime[_: P]: P[DateWithTime] =
    P(
      "[" ~ pUnsignedInt ~
        "-" ~ pUnsignedInt.filter(_ <= 12) ~
        "-" ~ pUnsignedInt.filter(_ <= 31) ~
        " " ~ pUnsignedInt.filter(_ <= 24) ~
        ":" ~ pUnsignedInt.filter(_ <= 60) ~ "]").map((DateWithTime.apply _).tupled)

  case class DateWithTime(year: Int, month: Int, day: Int, hour: Int, minute: Int) extends Ordered[DateWithTime] {

    import DateWithTime._

    val weight = (year * Year) + (month * Month) + (day * Day) + (hour * Hour) + minute

    override def compare(that: DateWithTime): Int = {
      weight - that.weight
    }

    def f(n: Int) = f"$n%02d"

    override def toString: String = s"[$year-${f(month)}-${f(day)} ${f(hour)}:${f(minute)}]"
  }

  object DateWithTime {
    val Hour = 60
    val Day = 24 * Hour
    val Month = 31 * Day
    val Year = 12 * Month
  }

}
