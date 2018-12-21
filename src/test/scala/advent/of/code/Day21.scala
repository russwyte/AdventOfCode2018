package advent.of.code
import scala.collection.mutable

class Day21 extends Day(21) {
  // I went straight to reverse engineering because I didn't want to waste time on the faulty device :)
  def solutions: Seq[Int] = {
    val rs: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet.empty
    val r0                             = 0
    var r4                             = 0
    var r2                             = 0
    var r1                             = 0
    var lte256                         = true
    var adding                         = true
    do {
      r1 = r4 | 65536
      r4 = 16031208
      do {
        r2 = r1 & 255
        r4 += r2
        r4 &= 16777215
        r4 *= 65899
        r4 &= 16777215
        lte256 = 256 <= r1
        if (lte256) r1 /= 256
      } while (lte256)
      adding = rs.add(r4)
    } while (r4 != r0 && adding)
    rs.toSeq
  }
  solutions.head should be(10720163)
  solutions.last should be(5885821)
}
