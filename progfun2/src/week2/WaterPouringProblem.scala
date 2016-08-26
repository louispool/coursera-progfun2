package week2

object WaterPouringProblem {

  class Pouring(capacity:  Vector[Int]) {

    //States
    type State = Vector[Int]
    val initialState = capacity.map(x => 0) //Empty

    //Moves
    trait Move {
      def change(state: State): State //State change operation
    }

    case class Empty(glass: Int) extends Move {
      def change(state: State) = state.updated(glass, 0) //Empties the glass
    }

    case class Fill(glass: Int) extends Move {
      def change(state: State) = state.updated(glass, capacity(glass)) //Fills the glass to capacity
    }

    case class Pour(from: Int, to: Int) extends Move {
      def change(state: State) = {
        val amount = state(from).min(capacity(to) - state(to))
        state.updated(from, state(from) - amount).updated(to, state(to) + amount) //Pours from one glass to another
      }
    }

    val glasses = 0 until capacity.length //all glasses
    val moves = {                         //all possible moves
      (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))
    }

    class Path(history: List[Move]) {
      def endState: State = trackState(history) // equivalent to history.foldRight(initialState)(_ change _)

      private def trackState(xs: List[Move]): State = xs match {
        case Nil => initialState
        case move :: xs1 => move.change(trackState(xs1)) //Last move is first in list
      }

      def extend(move: Move) = new Path(move :: history)

      override def toString = history.reverse.mkString(" ") + "--> " + endState
    }

    val initialPath = new Path(Nil) //Empty path

    //Generate successive paths
    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves.map(path.extend) //Extend current path with all possible moves
          if !explored.contains(next.endState) //Performance enhancement, do not recurse states we have already visited
        } yield next
        paths #:: from(more, explored ++ more.map(_.endState))
      }
    }

    val pathSets = from(Set(initialPath), Set(initialState))

    def solution(target: Int): Stream[Path] = {
      for {
        pathSet <- pathSets
        path <- pathSet
        if path.endState.contains(target)
      } yield path
    }
  }

  def main(args: Array[String]) {
    val problem = new Pouring(Vector(4, 9))
    println(problem.moves)
    println(problem.pathSets.take(3).toList)
    println(problem.solution(6))
  }
}
