package learningGraph

import scala.collection.mutable

object WaterJug extends App {
  def canMeasureWater(jug1Capacity: Int, jug2Capacity: Int, targetCapacity: Int): Boolean = {
    var a = 0
    var b = 0
    var queue = List((a, b))
    //creates an empty Set in Scala, which contains tuples of two integers
    var visited = Set[(Int, Int)]()
    visited += ((a, b))

    while (queue.nonEmpty) {
      val (p, q) = queue.head
      //returns a new queue that contains all the elements of the original queue except for the head element
      queue = queue.tail

      if (p + q == targetCapacity) {
        return true
      }
      //Actions that we need to perform
      //Fill Jug1, Fill Jug2, Empty Jug1, Empty Jug2, Pour Jug 1 to Jug 2", Pour Jug 2 to Jug 1"
      val states = List((jug1Capacity, q), (p, jug2Capacity), (0, q), (p, 0),
        (p - math.min(p, jug2Capacity - q), q + math.min(p, jug2Capacity - q)),
        (p + math.min(q, jug1Capacity - p), q - math.min(q, jug1Capacity - p)))

      for (state <- states) {
        if (!visited.contains(state)) {
          //equivalent to enqueue
          //queue = queue :+ state
          // adds the state element to the end of the queue
          queue :+= state
          visited += state
        }
      }
    }

    false
  }
  println(canMeasureWater(3,5,4))
  println(canMeasureWater(2,6,5))
}
