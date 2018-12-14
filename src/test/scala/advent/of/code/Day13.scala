package advent.of.code
import scala.annotation.tailrec

class Day13 extends Day(13) {
  sealed trait Turn
  case object Left     extends Turn
  case object Right    extends Turn
  case object Straight extends Turn

  case class System(tracks: Grid, carts: Seq[Cart]) {
    def tick = {
      val c = carts.sortBy(c => (c.position.y, c.position.x)).map(_.advance(tracks))
      copy(carts = c)
    }
    def tick2 = {
      @tailrec
      def inner(left: List[Cart], moved: List[Cart]): List[Cart] = {
        left match {
          case Nil => moved
          case c :: cs =>
            val c2 = c.advance(tracks)
            if (cs.exists(_.position == c2.position)) inner(cs.filter(_.position != c2.position), moved)
            else {
              val ys =
                if (moved.exists(_.position == c2.position)) moved.filter(_.position != c2.position) else c2 :: moved
              inner(cs, ys)
            }
        }
      }
      this.copy(carts = inner(carts.sortBy(c => (c.position.y, c.position.x)).toList, Nil))
    }
    def firstWreck: Point = {
      @tailrec
      def inner(s: System): Point = {
        s.carts.groupBy(_.position).values.find(_.length > 1).map(_.head.position) match {
          case Some(p) => p
          case None    => inner(s.tick)
        }
      }
      inner(this)
    }
    def lastCart: Cart = {
      @tailrec
      def inner(s: System): Cart = {
        if (s.carts.length > 1) inner(s.tick2) else s.carts.head
      }
      inner(this)
    }
  }
  object System {
    def apply(input: String): System = {
      new System(Grid(
                   input
                     .replace(North.c, '|')
                     .replace(South.c, '|')
                     .replace(West.c, '-')
                     .replace(East.c, '-')),
                 Grid(input).carts)
    }
  }
  case class Grid(vector: Vector[Vector[Char]]) {
    def carts: Seq[Cart] = {
      for {
        y <- vector.indices
        x <- vector(y).indices
        c <- Direction(vector(y)(x)).map(d => Cart(Point(x, y), d))
      } yield {
        c
      }
    }
  }
  object Grid {
    def apply(s: String) = new Grid(s.linesIterator.map(_.toVector).toVector)
  }

  sealed trait Direction {
    def move: Point
    def c: Char
    def turn(t: Turn): Direction
    def next(c: Char, turns: Iterator[Turn]): Direction = {
      turn(c match {
        case '/'  => if (move.y == 0) Left else Right
        case '\\' => if (move.y == 0) Right else Left
        case '+'  => turns.next()
        case _    => Straight
      })
    }
  }
  case object North extends Direction {
    override def move: Point = Point(0, -1)
    override def c: Char     = '^'
    override def turn(t: Turn): Direction = {
      t match {
        case Left     => West
        case Right    => East
        case Straight => this
      }
    }
  }
  case object South extends Direction {
    override def move: Point = Point(0, 1)
    override def c: Char     = 'v'
    override def turn(t: Turn): Direction = {
      t match {
        case Left     => East
        case Right    => West
        case Straight => this
      }
    }
  }
  case object East extends Direction {
    override def move: Point = Point(1, 0)
    override def c: Char     = '>'
    override def turn(t: Turn): Direction = {
      t match {
        case Left     => North
        case Right    => South
        case Straight => this
      }
    }
  }
  case object West extends Direction {
    override def move: Point = Point(-1, 0)
    override def c: Char     = '<'
    override def turn(t: Turn): Direction = {
      t match {
        case Left     => South
        case Right    => North
        case Straight => this
      }
    }
  }
  object Direction {
    val Directions: Map[Char, Direction]  = Map(North.c -> North, South.c -> South, East.c -> East, West.c -> West)
    def apply(c: Char): Option[Direction] = Directions.get(c)
  }

  case class Cart(position: Point,
                  direction: Direction,
                  turns: Iterator[Turn] = Iterator.continually(List(Left, Straight, Right)).flatten) {
    def advance(grid: Grid): Cart = {
      val p = position + direction.move
      val c = grid.vector(p.y)(p.x)
      val d = direction.next(c, turns)
      copy(position = p, direction = d)
    }
  }

  val Sample  = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "
  val Sample2 = "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"

  "Part1" should "Work" in {
    System(Sample).firstWreck should be(Point(7, 3))
    System(raw).firstWreck should be(Point(117, 62))
  }
  "Part2" should "Work" in {
    System(Sample2).lastCart.position should be(Point(6, 4))
    System(raw).lastCart.position should be(Point(69, 67))
  }
}
