package learningGraph
import scala.collection.mutable.*
object BoatPuzzle extends App{
  // case class is a special type of class that is primarily used to hold immutable data.
  case class State(leftMissionaries: Int, leftCannibals: Int, boatOnLeft: Boolean) {
    def isValid: Boolean =
      if (leftMissionaries < leftCannibals && leftMissionaries > 0) false
      else if (leftMissionaries > 3 || leftCannibals > 3) false
      else true

    def isGoal: Boolean = leftMissionaries == 0 && leftCannibals == 0 && !boatOnLeft

    //Seq[State] is a type in Scala that represents an ordered sequence of elements, where each element is of type State
    def next: Seq[State] = {
      // boat has max. capacity of 2
      // but it can contain any 1 also
      val possibleMoves = Seq((2, 0), (1, 1), (0, 2), (1, 0), (0, 1))
      for {
        (missionariesMoved, cannibalsMoved) <- possibleMoves
        (newLeftMissionaries, newLeftCannibals, newBoatOnLeft) =
          if (boatOnLeft) {
            (
              leftMissionaries - missionariesMoved,
              leftCannibals - cannibalsMoved,
              false
            )
          } else {
            (
              leftMissionaries + missionariesMoved,
              leftCannibals + cannibalsMoved,
              true
            )
          }
        nextState = State(newLeftMissionaries, newLeftCannibals, newBoatOnLeft)
        if nextState.isValid
      } yield nextState
      //yield keyword is used to collect all these nextState objects into a new Seq[State] object,
      // which represents all possible next states from the current state
    }
  }
  //Option[List[State]] type is used to represent the path from the starting state to the goal state
  //optional value that can either be Some(value) or None. It's often used to handle cases where a value may or may not be present.
  def bfs(start: State): Option[List[State]] = {
    val queue = Queue[(State, List[State])]()
    queue.enqueue((start, List(start)))
    val visited = Set[State]()
    visited += start
    while (queue.nonEmpty) {
      val (current, path) = queue.dequeue()
      if (current.isGoal) return Some(path.reverse)
      for (next <- current.next if !visited.contains(next)) {
        //::  prepend an element to a sequence to create a new sequence
        queue.enqueue((next, next :: path))
        visited += next
      }
    }
    None
  }

  val initialState = State(3, 3, true)
  val solution = bfs(initialState)
  // checking whether an option instance contains a value or not
  if (solution.isDefined) {
    println(solution.get.mkString("\n"))
  } else {
    println("No solution found.")
  }

}
