package learningGraph
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DFSTraversal extends App{
  class Graph(val adjList: Map[Int, List[Int]]) {
    def dfs(start: Int): List[Int] = {
      val visited = mutable.Set[Int]()
      val result = ListBuffer[Int]()

      def dfsHelper(node: Int): Unit = {
        //available in mutable set collection only
        visited.add(node)
        result += node

        for (neighbor <- adjList.getOrElse(node, Nil)) {
          if (!visited.contains(neighbor)) {
            dfsHelper(neighbor)
          }
        }
      }

      dfsHelper(start)
      result.toList
    }
  }

  val graph = new Graph(Map(
    1 -> List(2, 3),
    2 -> List(1, 4),
    3 -> List(1, 4, 5),
    4 -> List(2, 3, 6),
    5 -> List(3, 6),
    6 -> List(4, 5, 7),
    7 -> List(6)
  ))

  val visited = graph.dfs(1)
  println(visited) // Output: List(1, 2, 4, 3, 5, 6, 7)
}
