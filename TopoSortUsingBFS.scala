package learningGraph
import scala.collection.mutable

object TopoSortUsingBFS extends App{
  class Graph(val adjList: Map[Int, List[Int]]) {
    def topologicalSort(): List[Int] = {
      val inDegrees = Array.fill(adjList.size)(0)
      val queue = mutable.Queue[Int]()
      val result = mutable.ListBuffer[Int]()

      for ((node, neighbors) <- adjList; neighbor <- neighbors) {
        inDegrees(neighbor) += 1
      }

      for (i <- inDegrees.indices) {
        if (inDegrees(i) == 0) {
          queue.enqueue(i)
        }
      }

      while (queue.nonEmpty) {
        val node = queue.dequeue()
        result += node

        for (neighbor <- adjList.getOrElse(node, Nil)) {
          inDegrees(neighbor) -= 1
          if (inDegrees(neighbor) == 0) {
            queue.enqueue(neighbor)
          }
        }
      }

      if (result.length != adjList.size) {
        throw new IllegalArgumentException("Graph has a cycle!")
      }

      result.toList
    }
  }

  val graph = new Graph(Map(
    0 -> List(1, 2),
    1 -> List(3),
    2 -> List(3),
    3 -> Nil
  ))
  val topologicalOrder = graph.topologicalSort()
  println(topologicalOrder) // Output: List(0, 1, 2, 3)
}
