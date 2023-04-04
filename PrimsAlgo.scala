package learningGraph

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object  PrimsAlgo extends App {
  class Pair(val distance: Int, val node: Int)
  //Function to find sum of weights of edges of the Minimum Spanning Tree.
  def spanningTree(V: Int, adj: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]): Int = {
    val pq = mutable.PriorityQueue.empty[Pair](Ordering.by(_.distance))

    val vis = Array.fill(V)(0)
    // {wt, node}
    pq.enqueue(new Pair(0, 0))
    var sum = 0
    while (pq.nonEmpty) {
      val wt = pq.head.distance
      val node = pq.head.node
      pq.dequeue()

      if (vis(node) == 1) ()
      else {
        // add it to the mst
        vis(node) = 1
        sum += wt

        for (i <- 0 until adj(node).size) {
          val edW = adj(node)(i)(1)
          val adjNode = adj(node)(i)(0)
          if (vis(adjNode) == 0) {
            pq.enqueue(new Pair(edW, adjNode))
          }
        }
      }
    }
    sum
  }

    val V = 5
    val adj = ArrayBuffer.fill(V)(ArrayBuffer.fill(0)(ArrayBuffer.fill(0)(0)))
    val edges = Array(Array(0, 1, 2), Array(0, 2, 1), Array(1, 2, 1), Array(2, 3, 2), Array(3, 4, 1), Array(4, 2, 2))

    for (i <- 0 until V) {
      adj(i) = ArrayBuffer.fill(0)(ArrayBuffer.fill(0)(0))
    }

    for (i <- 0 until 6) {
      val u = edges(i)(0)
      val v = edges(i)(1)
      val w = edges(i)(2)

      val tmp1 = ArrayBuffer(v, w)
      val tmp2 = ArrayBuffer(u, w)

      adj(u) += tmp1
      adj(v) += tmp2
    }

    val sum = spanningTree(V, adj)
    println(s"The sum of all the edge weights: $sum")


}

