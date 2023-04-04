package learningGraph

import scala.collection.mutable.PriorityQueue

object DijkstraAlgo extends App {
  class Solution {
    // Function to find the shortest distance of all the vertices
    // from the source vertex S.
    def dijkstra(V: Int, adj: Array[Array[(Int, Int)]], S: Int): Array[Int] = {
      // Create a priority queue for storing the nodes as a tuple (dist,node)
      // where dist is the distance from source to the node.
      val pq = PriorityQueue[(Int, Int)]()(Ordering.by(_._1))

      // Initialising distTo list with a large number to
      // indicate the nodes are unvisited initially.
      // This list contains distance from source to the nodes.
      val distTo = Array.fill(V)(Int.MaxValue)

      // Source initialised with dist=0.
      distTo(S) = 0
      pq.enqueue((0, S))

      // Now, dequeue the minimum distance node first from the min-heap
      // and traverse for all its adjacent nodes.
      while (pq.nonEmpty) {
        val (dis, node) = pq.dequeue()

        // Check for all adjacent nodes of the dequeued
        // element whether the prev dist is larger than current or not.
        for ((v, w) <- adj(node)) {
          if (dis + w < distTo(v)) {
            distTo(v) = dis + w

            // If current distance is smaller,
            // enqueue it into the queue.
            pq.enqueue((dis + w, v))
          }
        }
      }
      // Return the array containing shortest distances
      // from source to all the nodes.
      distTo
    }
  }

  // Define the number of vertices in the graph
  val V = 5

  // Define the adjacency list for the graph
  val adj = Array.ofDim[List[(Int, Int)]](V)
  adj(0) = List((1, 9), (2, 6), (3, 5), (4, 3))
  adj(1) = List((3, 1))
  adj(2) = List((1, 2), (3, 2))
  adj(3) = List((4, 4))
  adj(4) = List()

  // Define the source vertex
  val S = 0

  // Create an instance of the Solution class
  val sol = new Solution()

  // Call the dijkstra function to get the shortest distances
  val adjArray = adj.map(_.toArray).toArray
  val distTo = sol.dijkstra(V, adjArray, S)

  // Print the shortest distances from the source vertex
  println("Shortest distances from the source vertex:")
  for (i <- 0 until V) {
    println(s"Vertex $i: ${distTo(i)}")
  }

}
