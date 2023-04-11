package org.renaissance.graph

import scala.util.Random

import org.renaissance.Benchmark
import org.renaissance.Benchmark._
import org.renaissance.BenchmarkContext
import org.renaissance.BenchmarkResult
import org.renaissance.BenchmarkResult.Validators
import org.renaissance.License
import scala.collection.mutable

@Name("graph-traversal")
@Group("graph")
@Summary("Performs multiple graph traversal algorithms on a Barabási–Albert generated graph.")
@Licenses(Array(License.APACHE2))
@Repetitions(50)
@Parameter(name = "graph_size", defaultValue = "1500")
@Parameter(name = "initial_connections", defaultValue = "2")
@Configuration(name = "test", settings = Array("graph_size = 1000", "initial_connections = 2"))
@Configuration(name = "jmh")
final class GraphTraversal extends Benchmark {
  private var graphSize: Int = _
  private var initialConnections: Int = _
  private var graph: Map[Int, List[Int]] = _

  private val randomSeed = 42
  private val random = new Random(randomSeed)
  
  override def setUpBeforeAll(c: BenchmarkContext): Unit = {
    graphSize = c.parameter("graph_size").toPositiveInteger
    initialConnections = c.parameter("initial_connections").toPositiveInteger
    graph = generateGraph(graphSize, initialConnections)
  }

private def generateGraph(size: Int, initialConnections: Int): Map[Int, List[Int]] = {
    val graph = mutable.Map[Int, List[Int]]().withDefaultValue(List.empty[Int])
    
    graph(0) = List(1)
    graph(1) = List(0)

    for (newNode <- 2 until size) {
      val degreeSum = graph.values.flatten.size
      val edgeProbabilities = graph.map { case (node, neighbors) =>
        (node, neighbors.size.toDouble / degreeSum)
      }.toSeq

      val selectedNodes = mutable.Set.empty[Int]
      while (selectedNodes.size < initialConnections) {
        val randomValue = random.nextDouble()
        var accumulatedProbability = 0.0

        for ((node, probability) <- edgeProbabilities) {
          accumulatedProbability += probability
          if (randomValue <= accumulatedProbability && selectedNodes.size < initialConnections) {
            selectedNodes.add(node)
          }
        }
      }

      graph(newNode) = selectedNodes.toList
      selectedNodes.foreach { node =>
        graph(node) = newNode :: graph(node)
      }
    }

    graph.toMap
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