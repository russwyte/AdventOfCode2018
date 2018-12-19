package advent.of.code
import scala.annotation.tailrec

class Day19 extends Day(19) {
  def parseState(s: String): State = {
    val lines = s.trim.linesIterator.toList
    val ip    = lines.head.last - '0'
    State(ip,
          lines.tail
            .map(s => {
              val ss = s.split(" ")
              Call(ss.head, ss(1).toInt, ss(2).toInt, ss(3).toInt)
            })
            .toArray)
  }
  case class State(ip: Int, registers: Array[Int], program: Array[Call]) {
    @tailrec
    final def run: State = {
      val ipVal = registers(ip)
      if (ipVal == 2 && registers(1) != 0) {
        if (registers(2) % registers(1) == 0) {
          registers(0) = registers(0) + registers(1)

        }
        registers(3) = 0
        registers(5) = registers(2)
        registers(ip) = 12
        this.run
      } else {
        val res      = program(ipVal)(this)
        val newIpVal = res.ipVal + 1
        if (newIpVal >= program.length) res
        else {
          res.registers(ip) = res.ipVal + 1
          res.run
        }
      }
    }
    def ipVal = registers(ip)
    def apply(r: Int, v: Int): State = {
      val res = this.copy()
      res.registers(r) = v
      res
    }
  }

  object State {
    def apply(ip: Int, program: Array[Call]): State = new State(ip, Array[Int](0, 0, 0, 0, 0, 0), program)
  }

  case class Call(opCode: String, a: Int, b: Int, c: Int) {
    def apply(state: State): State = instructions(opCode)(state, a, b, c)
  }
  type Op = (Int, Int) => Int

  type Instruction = (State, Int, Int, Int) => State

  def rr(op: Op): Instruction = (s, a, b, c) => s(c, op(s.registers(a), s.registers(b)))
  def ri(op: Op): Instruction = (s, a, b, c) => s(c, op(s.registers(a), b))
  def ir(op: Op): Instruction = (s, a, b, c) => s(c, op(a, s.registers(b)))
  def ii(op: Op): Instruction = (s, a, b, c) => s(c, op(a, b))

  val instructions: Map[String, Instruction] = Map(
    "addr" -> rr(_ + _),
    "addi" -> ri(_ + _),
    "mulr" -> rr(_ * _),
    "muli" -> ri(_ * _),
    "banr" -> rr(_ & _),
    "bani" -> ri(_ & _),
    "borr" -> rr(_ | _),
    "bori" -> ri(_ | _),
    "setr" -> ri((a, _) => a),
    "seti" -> ii((a, _) => a),
    "gtir" -> ir((a, b) => if (a > b) 1 else 0),
    "gtri" -> ri((a, b) => if (a > b) 1 else 0),
    "gtrr" -> rr((a, b) => if (a > b) 1 else 0),
    "eqir" -> ir((a, b) => if (a == b) 1 else 0),
    "eqri" -> ri((a, b) => if (a == b) 1 else 0),
    "eqrr" -> rr((a, b) => if (a == b) 1 else 0)
  )

  "The program" should "run" in {
    // part1 was 1922
//    parseState("#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5").run
//      .registers(0) should be(6)
    parseState(raw).run.registers(0) should be(1922)
    parseState(raw)(0, 1).run.registers(0) should be(22302144)
  }
}
