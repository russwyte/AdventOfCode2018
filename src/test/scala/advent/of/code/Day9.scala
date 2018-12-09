package advent.of.code
import org.scalatest.{FlatSpec, Matchers}

class Day9 extends FlatSpec with Matchers {
  case class Game(players: Array[Long], circle: CircleZipper[Int] = CircleZipper(0)) {
    def apply(marble: Int): Game = {
      val playerNumber = marble % players.length
      if (marble % 23 == 0) {
        val c = circle.rotate(-7)
        players(playerNumber) = players(playerNumber) + c.focus + marble
        copy(circle = c.remove)
      } else {
        val c = circle.rotate(2).insert(marble)
        copy(circle = c)
      }
    }
    def winningScore: Long = players.max
  }
  object Game {
    def apply(numberOfPlayers: Int, numberOfMarbles: Int): Game = {
      (1 to numberOfMarbles).foldLeft(Game(Array.fill(numberOfPlayers)(0)))((g, m) => g.apply(m))
    }
  }
  "part 1 and 2" should "work" in {
    Game(9, 25).winningScore should be(32)
    Game(10, 1618).winningScore should be(8317)
    Game(13, 7999).winningScore should be(146373)
    Game(17, 1104).winningScore should be(2764)
    Game(21, 6111).winningScore should be(54718)
    Game(30, 5807).winningScore should be(37305)
    Game(432, 71019).winningScore should be(400493)
    Game(432, 7101900).winningScore should be(3338341690L)
  }
}
