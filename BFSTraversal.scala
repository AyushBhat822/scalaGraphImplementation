package learningGraph
import scala.collection.mutable


object BFSTraversal extends App{
  class Graph(val adjList: Map[Int, List[Int]]) {
    def bfs(start: Int): List[Int] = {
      val visited = Array.fill(adjList.size)(false)
      val queue = mutable.Queue[Int]()
      val result = mutable.ListBuffer[Int]()

      visited(start) = true
      queue.enqueue(start)

      while (queue.nonEmpty) {
        val node = queue.dequeue()
        result += node

        for (neighbor <- adjList.getOrElse(node, Nil)) {
          if (!visited(neighbor)) {
            visited(neighbor) = true
            queue.enqueue(neighbor)
          }
        }
      }

      result.toList
    }
  }

  val graph = new Graph(Map(
    0 -> List(1, 2),
    1 -> List(2, 3),
    2 -> List(3),
    3 -> Nil
  ))

  val bfsOrder = graph.bfs(0)
  println(bfsOrder) // Output: List(0, 1, 2, 3)
}
