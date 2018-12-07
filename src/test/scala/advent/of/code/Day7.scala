package advent.of.code

import fastparse.NoWhitespace._
import fastparse._

class Day7 extends Day(7) {

  type Step      = Char
  type StepPaths = Map[Step, Set[Step]]

  case class Order(preceding: Step, following: Step)

  def pStep[_: P]: P[Step] = P(CharPred(c => CharPredicates.isLetter(c) && CharPredicates.isUpper(c)).!).map(_.head)

  def pOrder[_: P]: P[Order] =
    P("Step " ~ pStep ~ " must be finished before step " ~ pStep ~ " can begin." ~ End)
      .map {
        Order.tupled
      }

  def stepPaths(orders: Seq[Order]): StepPaths = orders.foldLeft[StepPaths](Map.empty) { (m, o) =>
    val ps = m.getOrElse(o.preceding, Set()) + o.following
    val fs = m.getOrElse(o.following, Set())
    m + (o.preceding -> ps) + (o.following -> fs)
  }
  def part1(orders: Seq[Order]): Seq[Step] = {
    def sort(ps: StepPaths, reversed: List[Step]): Seq[Step] = {
      ps.values.reduceOption(_ ++ _) match {
        case None => reversed.reverse
        case Some(o) =>
          val step = (ps.keySet -- o).min
          val rest = ps.filterKeys(_ != step)
          sort(rest, step :: reversed)
      }
    }
    sort(stepPaths(orders), List.empty)
  }

  def part2(orders: Seq[Order], workers: Int, timeBase: Int): Int = {

    case class Task(step: Step, remaining: Int) {
      def advance(time: Int): Task = copy(remaining = remaining - time)
    }

    object Task {
      def apply(step: Step): Task = Task(step, timeBase + (step - 'A' + 1))
    }

    case class Progress(remaining: StepPaths, tasks: Set[Task], timeStep: Int) {
      def finished: Boolean = remaining.isEmpty && tasks.isEmpty
      def assignTasks: Progress = {
        val assignedTasks = remaining.values.reduceOption(_ ++ _) match {
          case None => Nil
          case Some(haveReqStep) =>
            val steps    = tasks.map(_.step)
            val possible = remaining.keySet -- haveReqStep -- steps
            val assigned = possible.toList.sorted.take(workers - tasks.size)
            assigned.map(step => Task(step))
        }
        Progress(remaining, tasks ++ assignedTasks)
      }
    }

    object Progress {
      def apply(paths: StepPaths, workQueue: Set[Task]): Progress = {
        val timeStep: Int  = workQueue.map(_.remaining).min
        val andvancedTasks = workQueue.map(_.advance(timeStep))

        val (finished: Set[Task], tasks: Set[Task]) = andvancedTasks.partition(_.remaining == 0)
        val finishedSteps: Set[Step]                = finished.map(_.step)
        val remaining: StepPaths                    = paths -- finishedSteps

        new Progress(remaining, tasks, timeStep)
      }
      def apply(paths: StepPaths): Progress = {
        new Progress(paths, Set.empty, 0)
      }
    }

    def inner(progress: Progress, time: Int): Int = {
      if (progress.finished) time
      else {
        val p = progress.assignTasks
        inner(progress.assignTasks, time + p.timeStep)
      }
    }

    inner(Progress(stepPaths(orders)), 0)
  }

  val Sample = Seq(Order('C', 'A'),
                   Order('C', 'F'),
                   Order('A', 'B'),
                   Order('A', 'D'),
                   Order('B', 'E'),
                   Order('D', 'E'),
                   Order('F', 'E'))
  "Day 7" should "go well" in {
    part1(Sample).mkString should be("CABDFE")
    part1(input.map(parse(_, pOrder(_)).get.value)).mkString should be("BITRAQVSGUWKXYHMZPOCDLJNFE")
    part2(Sample, 2, 0) should be(15)
    part2(input.map(parse(_, pOrder(_)).get.value), 5, 60) should be(869)
  }
}
