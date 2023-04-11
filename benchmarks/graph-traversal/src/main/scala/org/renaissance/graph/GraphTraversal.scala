package org.renaissance.graph

import org.renaissance.Benchmark
import org.renaissance.Benchmark._
import org.renaissance.BenchmarkContext
import org.renaissance.BenchmarkResult
import org.renaissance.BenchmarkResult.Validators
import org.renaissance.License
import scala.collection.mutable

@Name("graph-traversal")
@Group("graph")
@Summary("Performs multiple graph traversal algorithms on an Erdős–Rényi generated graph.")
@Licenses(Array(License.APACHE2))
@Repetitions(50)
@Parameter(name = "graph_size", defaultValue = "1500")
@Parameter(name = "edge_probability", defaultValue = "0.5")
@Configuration(name = "test", settings = Array("graph_size = 1000", "edge_probability = 0.5"))
@Configuration(name = "jmh")
final class GraphTraversal extends Benchmark {
  private var graphSize: Int = _
  private var edgeProbability: Double = _
  private var graph: Map[Int, List[Int]] = _

  override def setUpBeforeAll(c: BenchmarkContext): Unit = {
    graphSize = c.parameter("graph_size").toPositiveInteger
    edgeProbability = c.parameter("edge_probability").toDouble
    graph = generateGraph(graphSize, edgeProbability)
  }

  private def generateGraph(size: Int, edgeProbability: Double): Map[Int, List[Int]] = {
    (0 until size).map { node =>
      val neighbors = (0 until size).filter(_ != node).filter(_ => scala.util.Random.nextDouble() <= edgeProbability).toList
      (node, neighbors)
    }.toMap
  }

  private def bfs(graph: Map[Int, List[Int]], startNode: Int): List[Int] = {
    val visited = Array.fill(graphSize)(false)
    val queue = mutable.Queue(startNode)

    var traversalOrder = List.empty[Int]

    while (queue.nonEmpty) {
      val currentNode = queue.dequeue()
      if (!visited(currentNode)) {
        visited(currentNode) = true
        traversalOrder = traversalOrder :+ currentNode

        val neighbors = graph(currentNode)
        neighbors.filterNot(visited).foreach(neighbor => queue.enqueue(neighbor))
      }
    }

    traversalOrder
  }

    private def dfs(graph: Map[Int, List[Int]], startNode: Int): List[Int] = {
      def dfsRecursive(node: Int, visited: Array[Boolean], traversalOrder: List[Int]): List[Int] = {
        if (visited(node)) {
          traversalOrder
        } else {
          visited(node) = true
          val newTraversalOrder = traversalOrder :+ node
          val unvisitedNeighbors = graph(node).filterNot(visited)

          unvisitedNeighbors.foldLeft(newTraversalOrder) { (order, neighbor) =>
            dfsRecursive(neighbor, visited, order)
          }
        }
      }

      val visited = Array.fill(graphSize)(false)
      dfsRecursive(startNode, visited, List.empty[Int])
    }
private def dijkstra(graph: Map[Int, List[Int]], startNode: Int): Map[Int, Int] = {
  val dist = mutable.Map[Int, Int]().withDefaultValue(Int.MaxValue)
  val visited = Array.fill(graphSize)(false)
  val queue = mutable.Queue(startNode)

  dist(startNode) = 0

  while (queue.nonEmpty) {
    val currentNode = queue.dequeue()
    if (!visited(currentNode)) {
      visited(currentNode) = true
      val neighbors = graph(currentNode)

      for (neighbor <- neighbors) {
        val newDist = dist(currentNode) + 1 
        if (newDist < dist(neighbor)) {
          dist(neighbor) = newDist
          queue.enqueue(neighbor)
        }
      }
    }
  }

  dist.toMap
}


  override def run(c: BenchmarkContext): BenchmarkResult = {
    val bfsStartTime = System.nanoTime()
    val bfsTraversalOrder = bfs(graph, 0)
    val bfsElapsedTime = System.nanoTime() - bfsStartTime
    println(s"Total elapsed time for bfs is: $bfsElapsedTime in nano")

    val dfsStartTime = System.nanoTime()
    val dfsTraversalOrder = dfs(graph, 0)
    val dfsElapsedTime = System.nanoTime() - dfsStartTime
    println(s"Total elapsed time for dfs is: $dfsElapsedTime in nano")

    val dijkstraStartTime = System.nanoTime()
    val dijkstraDistances = dijkstra(graph, 0)
    val dijkstraElapsedTime = System.nanoTime() - dijkstraStartTime
    println(s"Total elapsed time for dijkstra is: $dijkstraElapsedTime in nano")

    Validators.simple("bfsOrderSize", graphSize, bfsTraversalOrder.size)
    Validators.simple("dfsOrderSize", graphSize, dfsTraversalOrder.size)
    Validators.simple("dfsOrderSize", graphSize, dijkstraDistances.size)
  }

  override def tearDownAfterAll(c: BenchmarkContext): Unit = {
    graph = null
  }
}
