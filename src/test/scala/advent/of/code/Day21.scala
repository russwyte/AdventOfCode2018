package advent.of.code
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Day21 extends FlatSpec with Matchers {

  /**
    *   I went straight to reverse engineering because I didn't want to waste time on the faulty device :)
    *   personally - I am not a fan of this sort of puzzle because we end up coding a very specific solution
    *   for exactly one program input and not a general one.
    *
    **/
  lazy val solutions: Seq[Int] = {
    // need to preserve insert order
    val rs: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet.empty
    var r1, r4                         = 0
    var r1gt255, added                 = true
    do {
      r1 = r4 | 65536
      r4 = 16031208
      do {
        r4 += r1 & 255
        r4 &= 16777215
        r4 *= 65899
        r4 &= 16777215
        r1gt255 = r1 > 255
        r1 /= 256
      } while (r1gt255)
      added = rs.add(r4)
    } while (r4 != 0 && added)
    rs.toSeq
  }
  solutions.head should be(10720163)
  solutions.last should be(5885821)
}
