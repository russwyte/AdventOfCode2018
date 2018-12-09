package advent.of.code

case class CircleZipper[T](left: List[T], focus: T, right: List[T]) {
  def next: CircleZipper[T] = right match {
    case Nil =>
      left.reverse match {
        case Nil     => this
        case t :: ts => CircleZipper(List(focus), t, ts)
      }
    case t :: ts => CircleZipper(focus :: left, t, ts)
  }

  def previous: CircleZipper[T] = left match {
    case Nil =>
      right.reverse match {
        case Nil     => this
        case t :: ts => CircleZipper(ts, t, List(focus))
      }
    case t :: ts => CircleZipper(ts, t, focus :: right)
  }

  def rotate(n: Int): CircleZipper[T] = {
    if (n > 0) next.rotate(n - 1)
    else if (n < 0) previous.rotate(n + 1)
    else this
  }

  def insert(t: T): CircleZipper[T] = CircleZipper(left, t, focus :: right)
  def remove: CircleZipper[T] = right match {
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
