package learningGraph
import com.sun.corba.se.impl.orbutil.graph.Graph
import scala.collection.mutable.*
import scala.jdk.CollectionConverters.*
import scala.io.StdIn.*

object dayOne extends App{
  class Graph(isDirected: Boolean){
    // Initializing empty map to store adjacency list
    var adjList:Map[Int,List[Int]]=Map()

    //Function to add edges
    def addEdge(u:Int,v:Int) = {
      if(isDirected){
        adjList += u -> (v :: adjList.getOrElse(u,Nil))
      }
      else {
        adjList += u -> (v :: adjList.getOrElse(u,Nil))
        adjList += v -> (u :: adjList.getOrElse(v,Nil))
      }
    }

    //To print adjacency list
    def printAdjList(): Unit = {
      for((node,neighbors) <- adjList){
        println(s"$node ${neighbors.mkString(",")} ")
      }
    }
  }

  // Example usage for directed graph
  val directedGraph = new Graph(isDirected = true)
  directedGraph.addEdge(0, 1)
  directedGraph.addEdge(0, 2)
  directedGraph.addEdge(1, 3)
  directedGraph.addEdge(2, 4)
  directedGraph.addEdge(3, 4)
  println("Directed Graph:")
  directedGraph.printAdjList()

  // Example usage for undirected graph
  val undirectedGraph = new Graph(isDirected = false)
  undirectedGraph.addEdge(0, 1)
  undirectedGraph.addEdge(0, 2)
  undirectedGraph.addEdge(1, 3)
  undirectedGraph.addEdge(2, 4)
  undirectedGraph.addEdge(3, 4)
  println("\nUndirected Graph:")
  undirectedGraph.printAdjList()

}
