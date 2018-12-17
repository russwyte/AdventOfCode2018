package advent.of.code

import fastparse._
import NoWhitespace._

class Day16 extends Day(16) {
  case class Registers(r0: Int, r1: Int, r2: Int, r3: Int) {
    def get(r: Int): Option[Int] = {
      r match {
        case 0 => Some(r0)
        case 1 => Some(r1)
        case 2 => Some(r2)
        case 3 => Some(r3)
        case _ => None
      }
    }
    def set(r: Int, n: Int): Option[Registers] = {
      r match {
        case 0 => Some(copy(r0 = n))
        case 1 => Some(copy(r1 = n))
        case 2 => Some(copy(r2 = n))
        case 3 => Some(copy(r3 = n))
        case _ => None
      }
    }
    def apply(operation: Operation): Registers = {
      opMap(instructions(operation.opCode))(this, operation.a, operation.b, operation.c).get
    }
  }
  type Instruction = (Registers, Int, Int, Int) => Option[Registers]
  def rr(f: (Int, Int) => Int): Instruction = (r, a, b, c) => {
    r.get(a).flatMap(x => r.get(b).flatMap(y => r.set(c, f(x, y))))
  }
  def ri(f: (Int, Int) => Int): Instruction = (r, a, b, c) => {
    r.get(a).flatMap(x => r.set(c, f(x, b)))
  }
  def ir(f: (Int, Int) => Int): Instruction = (r, a, b, c) => {
    r.get(b).flatMap(x => r.set(c, f(a, x)))
  }

  val addr = rr(_ + _)
  val addi = ri(_ + _)

  val mulr = rr(_ * _)
  val muli = ri(_ * _)

  val banr = rr(_ & _)
  val bani = ri(_ & _)

  val borr = rr(_ | _)
  val bori = ri(_ | _)

  val setr              = rr((a, _) => a)
  val seti: Instruction = (r, a, b, c) => r.set(c, a)

  val gt: (Int, Int) => Int = (a, b) => if (a > b) 1 else 0

  val gtir = ir(gt)
  val gtri = ri(gt)
  val gtrr = rr(gt)

  val eq: (Int, Int) => Int = (a, b) => if (a == b) 1 else 0

  val eqir = ir(eq)
  val eqri = ri(eq)
  val eqrr = rr(eq)

  val opMap: Map[String, Instruction] = Map[String, Instruction](
    "addr" -> addr,
    "addi" -> addi,
    "mulr" -> mulr,
    "muli" -> muli,
    "banr" -> banr,
    "bani" -> bani,
    "borr" -> borr,
    "bori" -> bori,
    "setr" -> setr,
    "seti" -> seti,
    "gtir" -> gtir,
    "gtri" -> gtri,
    "gtrr" -> gtrr,
    "eqir" -> eqir,
    "eqri" -> eqri,
    "eqrr" -> eqrr
  )

  def check(i: Instruction, effect: Effect): Boolean =
    i(effect.input, effect.operation.a, effect.operation.b, effect.operation.c).contains(effect.output)

  val parts = raw.split("\n\n\n2 2 3 3")
  val raw1  = parts(0)
  val raw2  = "2 2 3 3\n" + parts(1).trim

  case class Operation(opCode: Int, a: Int, b: Int, c: Int) {
    def apply(instruction: Instruction, registers: Registers): Option[Registers] = instruction(registers, a, b, c)
  }

  case class Effect(input: Registers, operation: Operation, output: Registers)

  def pRegisters[_: P]: P[Registers] =
    P(("Before: " | "After:  ") ~ "[" ~ pUnsignedInt.rep(4, ", ", 4) ~ "]" ~ "\n").map(x =>
      Registers(x(0), x(1), x(2), x(3)))

  def pOperation[_: P]: P[Operation] =
    P(pUnsignedInt.rep(4, " ", 4) ~ "\n".?).map(x => Operation(x(0), x(1), x(2), x(3)))

  def pEffect[_: P]: P[Effect] = P(pRegisters ~ pOperation ~ pRegisters).map { Effect.tupled }

  def pEffects[_: P]: P[Seq[Effect]] = P(pEffect.rep(1, "\n"))

  lazy val effects: Seq[Effect] = parse(raw1, pEffects(_)).get.value

  def countMatchingInstructions(effect: Effect): Int                 = matchingInstructions(effect).size
  def matchingInstructions(effect: Effect): Map[String, Instruction] = opMap.filter(x => check(x._2, effect))

  lazy val grouped = effects.groupBy(_.operation.opCode)

  lazy val instructions: Map[Int, String] = {
    effects
      .foldLeft((0 to 15).map(_ -> opMap.keySet).toMap)({ (m, effect) =>
        val common = m(effect.operation.opCode) intersect matchingInstructions(effect).keySet
        val om     = m.updated(effect.operation.opCode, common)
        if (common.size == 1)
          om.mapValues(o => if (o == common) o else o -- common)
        else
          om
      })
      .mapValues(s => s.head)
  }

  println(instructions)

  "The example" should "work" in {
    parse("Before: [1, 2, 0, 1]\n", pRegisters(_)).get.value should be(Registers(1, 2, 0, 1))
    countMatchingInstructions(Effect(Registers(3, 2, 1, 1), Operation(9, 2, 1, 2), Registers(3, 2, 2, 1))) should be(3)
  }
  "part1" should "work" in {
    effects.map(countMatchingInstructions).count(_ >= 3) should be(640)
  }
  "part2" should "work" in {
    val ops = raw2.linesIterator.map(s => parse(s, pOperation(_)).get.value).toList
    ops.length should be(868)
    ops
      .foldLeft(Registers(0, 0, 0, 0))((r, o) => {
        r(o)
      })
      .r0 should be(472)
  }
}
