package learningGraph

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

object primsAlgoII extends App {
  // Function to find the minimum spanning tree
  def spanningTree(N: Int, E: Array[Array[Int]]): Array[(Int, Int)] = {
    // Initialize the adjacency list
    val adjList = Array.fill(N + 1)(ArrayBuffer[(Int, Int)]())
    // Fill the adjacency list
    for (i <- 0 until E.length) {
      adjList(E(i)(0)) += ((E(i)(1), E(i)(2)))
      adjList(E(i)(1)) += ((E(i)(0), E(i)(2)))
    }

    // Initialize the priority queue
    val pq = PriorityQueue.empty[(Int, Int)](Ordering.by(_._1))

    // Initialize the visited array and the minimum spanning tree
    val visited = Array.fill(N + 1)(false)
    val mst = ArrayBuffer[(Int, Int)]()

    // Add the first node to the priority queue
    visited(1) = true
    for (i <- 0 until adjList(1).length) {
      pq.enqueue(adjList(1)(i)._2 -> adjList(1)(i)._1)
    }

    // Loop until the priority queue is empty
    while (pq.nonEmpty) {
      // Get the smallest edge from the priority queue
      val (dist, node) = pq.dequeue()

      // If the node is already visited, skip it
      if (visited(node)) {
        // Otherwise, add the edge to the minimum spanning tree
      } else {
        visited(node) = true
        mst += ((node, dist))

        // Add the adjacent edges to the priority queue
        for (i <- 0 until adjList(node).length) {
          val (adjNode, adjDist) = adjList(node)(i)
          if (!visited(adjNode)) {
            pq.enqueue(adjDist -> adjNode)
          }
        }
      }
    }

    // Return the minimum spanning tree
    mst.toArray
  }

  val N = 5
  val E = Array(Array(1, 2, 1), Array(2, 3, 2), Array(3, 4, 3), Array(4, 5, 4), Array(5, 1, 5), Array(1, 3, 6), Array(3, 5, 7))
  val mst = spanningTree(N, E)

  // Print the minimum spanning tree
  println("Minimum Spanning Tree:")
  for (i <- 0 until mst.length) {
    println(s"${mst(i)._1} --- ${mst(i)._2}")
  }

}

