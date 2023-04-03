package learningGraph
import scala.collection.mutable

object TopoSortUsingDFS extends App {
  class Graph(val adjList: Map[Int, List[Int]]) {
    def topologicalSort(): List[Int] = {
      val visited = Array.fill(adjList.size)(false)
      val stack = mutable.Stack[Int]()
      for (node <- adjList.keys) {
        if (!visited(node)) {
          dfs(node, visited, stack)
        }
      }
      stack.toList
    }
    def dfs(node: Int, visited: Array[Boolean], stack: mutable.Stack[Int]): Unit = {
      visited(node) = true
      for (neighbor <- adjList.getOrElse(node, Nil)) {
        if (!visited(neighbor)) {
          dfs(neighbor, visited, stack)
        }
      }

      stack.push(node)
    }
  }

  val graph = new Graph(Map(
    0 -> List(1, 2),
    1 -> List(3),
    2 -> List(3),
    3 -> Nil
  ))

  val sorted = graph.topologicalSort()
  println(sorted) // Output: List(0, 2, 1, 3)
}
