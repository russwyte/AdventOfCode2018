package advent.of.code
import scala.annotation.tailrec
import fastparse._
import NoWhitespace._

class Day24 extends Day(24) {
  sealed trait Damage
  case object Bludgeoning extends Damage
  case object Cold        extends Damage
  case object Radiation   extends Damage
  case object Slashing    extends Damage
  case object Fire        extends Damage
  case class Group(units: Long,
                   unitHP: Int,
                   weaknesses: Set[Damage],
                   immunities: Set[Damage],
                   attack: Damage,
                   strength: Int,
                   initiative: Int) {
    def effectivePower: Long = units * strength
    def potentialDamage(attacker: Group): Long = {
      attacker.effectivePower * (if (weaknesses(attacker.attack)) 2 else if (immunities(attacker.attack)) 0 else 1)
    }
    def apply(damage: Long): Group = {
      val killed = damage / unitHP
      copy(units = units - killed)
    }
    override def toString: String =
      s"units:$units unitHP:$unitHP weaknesses:$weaknesses immunities:$immunities initiative: $initiative attack type:$attack strength:$strength ep:$effectivePower"
  }
  case class Army(groups: Array[Group], name: String) {
    def attackers: Array[Int] =
      groups.zipWithIndex
        .map(_._2)
        .filter(groups(_).units > 0)
        .sortBy(x => (-groups(x).effectivePower, -groups(x).initiative))
    def battles(other: Army): List[(Int, Int, Army, Army)] = {
      @tailrec
      def inner(as: List[Int], ds: List[Int], res: List[(Int, Int)]): List[(Int, Int)] = {
        as match {
          case Nil => res
          case a :: as =>
            val targets = ds
              .filter(x => other.groups(x).potentialDamage(groups(a)) > 0 && other.groups(x).units > 0)
              .sortBy { x =>
                val g = other.groups(x)
                (-g.potentialDamage(groups(a)), -g.effectivePower, -g.initiative)
              }
            targets match {
              case Nil    => inner(as, ds, res)
              case d :: _ => inner(as, ds.filterNot(_ == d), (a, d) :: res)
            }
        }
      }
      inner(attackers.toList, other.groups.indices.toList, Nil).sortBy(x => -groups(x._1).initiative).map { x =>
        (x._1, x._2, this, other)
      }
    }
  }

  case class Fight(army1: Army, army2: Army) {
    def turn = {
      val bs = (army1.battles(army2) ++ army2.battles(army1)).sortBy(x => -x._3.groups(x._1).initiative)
      bs.foreach {
        case (a, d, a1, a2) =>
          val ag = a1.groups(a)
          val dg = a2.groups(d)
          if (ag.units > 0) {
            a2.groups(d) = dg.apply(dg.potentialDamage(ag))
          }
      }
    }
    def pickWinner: Army = if (army1.attackers.length > army2.attackers.length) army1 else army2
    def winningUnits: Long = {
      val w = winningArmy
      w.groups.filter(_.units > 0).map(_.units).sum
    }
    def checksum: Long =
      army1.groups.filter(_.units > 0).map(_.units).sum + army2.groups.filter(_.units > 0).map(_.units).sum
    def winningArmy: Army = {
      var last   = -1L
      var winner = army2
      while (army1.attackers.length > 0 && army2.attackers.length > 0 && checksum != last) {
        last = checksum
        turn
      }
      if (army2.attackers.length == 0 || army1.attackers.length == 0) winner = pickWinner
      winner
    }
  }
  val immuneSample = Army(
    Array[Group](Group(17, 5930, Set(Radiation, Bludgeoning), Set.empty, Fire, 4507, 2),
                 Group(989, 1274, Set(Slashing, Bludgeoning), Set(Fire), Slashing, 25, 3),
    ),
    "immune"
  )
  val infectSample = Army(Array[Group](Group(801, 4706, Set(Radiation), Set(), Bludgeoning, 116, 1),
                                       Group(4485, 2961, Set(Fire, Cold), Set(Radiation), Slashing, 12, 4),
                          ),
                          "infection")
  def pDamge[_: P]: P[Damage] =
    P(P("bludgeoning" | "slashing" | "cold" | "fire" | "radiation").!.map {
      case "bludgeoning" => Bludgeoning
      case "slashing"    => Slashing
      case "cold"        => Cold
      case "fire"        => Fire
      case "radiation"   => Radiation
    })
  case class Weaknesses(set: Set[Damage])
  case class Immunities(set: Set[Damage])
  def pImmunities[_: P]: P[Immunities] = P("immune to " ~ pDamge.rep(1, ", ")).map(x => Immunities(x.toSet))

  def pWeaknesses[_: P]: P[Weaknesses] = P("weak to " ~ pDamge.rep(1, ", ")).map(x => Weaknesses(x.toSet))

  def modifiersIW[_: P]: P[(Immunities, Weaknesses)] = P(" (" ~ pImmunities ~ ("; " ~ pWeaknesses).? ~ ")").map {
    case (i, ow) => (i, ow.getOrElse(Weaknesses(Set.empty)))
  }
  def modifiersWI[_: P]: P[(Immunities, Weaknesses)] = P(" (" ~ pWeaknesses ~ ("; " ~ pImmunities).? ~ ")").map {
    case (w, oi) => (oi.getOrElse(Immunities(Set.empty)), w)
  }

  def pGroup[_: P] =
    P(
      pUnsignedInt ~ " units each with " ~ pUnsignedInt ~ " hit points"
        ~ (modifiersIW | modifiersWI).? ~ " with an attack that does " ~ pUnsignedInt ~ " "
        ~ pDamge ~ " damage at initiative " ~ pUnsignedInt).map {
      case (u, hp, owi, a, d, i) =>
        val x = owi.getOrElse((Immunities(Set.empty), Weaknesses(Set.empty)))
        Group(u, hp, x._2.set, x._1.set, d, a, i)
    }

  def pImmuneSystem[_: P]: P[Army] = P("Immune System:\n" ~ pGroup.rep(1, "\n")).map(x => Army(x.toArray, "immune"))
  def pInfection[_: P]: P[Army]    = P("\nInfection:\n" ~ pGroup.rep(1, "\n")).map(x => Army(x.toArray, "infection"))
  def pFight[_: P]: P[Fight]       = P(pImmuneSystem ~ "\n" ~ pInfection).map(Fight.tupled)
  def fight(s: String): Fight      = parse(s, pFight(_)).get.value
  def part2: Long = {
    def inner(boost: Int): Long = {
      val f  = fight(raw)
      val gs = f.army1.groups.map(g => g.copy(strength = g.strength + boost))
      val bf = f.copy(army1 = f.army1.copy(groups = gs))
      val a  = bf.winningArmy
      if (a.name == "immune") a.groups.map(_.units).sum else inner(boost + 1)
    }
    inner(1)
  }
  Fight(immuneSample, infectSample).winningUnits should be(5216)
  fight(raw).winningUnits should be(19295)
  part2 should be(12084)
}
