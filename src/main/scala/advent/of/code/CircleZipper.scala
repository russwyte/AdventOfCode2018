package advent.of.code
import scala.annotation.tailrec

case class CircleZipper[A](left: List[A], focus: A, right: List[A]) {
  def next: CircleZipper[A] = right match {
    case Nil =>
      left.reverse match {
        case Nil     => this
        case t :: ts => CircleZipper(List(focus), t, ts)
      }
    case t :: ts => CircleZipper(focus :: left, t, ts)
  }

  def previous: CircleZipper[A] = left match {
    case Nil =>
      right.reverse match {
        case Nil     => this
        case t :: ts => CircleZipper(ts, t, List(focus))
      }
    case t :: ts => CircleZipper(ts, t, focus :: right)
  }

  def rotate(n: Int): CircleZipper[A] = {
    if (n > 0) next.rotate(n - 1)
    else if (n < 0) previous.rotate(n + 1)
    else this
  }

  def insert(t: A): CircleZipper[A] = CircleZipper(left, t, focus :: right)

  def update(t: A): CircleZipper[A] = remove.insert(t)

  def foldLeft[B](b: B)(f: (B, CircleZipper[A]) => B): B = {
    @tailrec
    def inner(count: Int, z: CircleZipper[A], b: B): B = {
      if (count == 0) b
      else {
        inner(count - 1, z.next, f(b, z))
      }
    }
    inner(length, this, b)
  }

  def length = 1 + left.length + right.length

  def remove: CircleZipper[A] = right match {
    case Nil =>
      left match {
        case Nil     => this
        case t :: ts => CircleZipper(ts, t, right)
      }
    case t :: ts => CircleZipper(left, t, ts)
  }
  override def toString: String = s"${left.mkString(" ")}($focus)${right.mkString(" ")}"
}
object CircleZipper {
  def apply[T](t: T): CircleZipper[T] = CircleZipper(Nil, t, Nil)
}
