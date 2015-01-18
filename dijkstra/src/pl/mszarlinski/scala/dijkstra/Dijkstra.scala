

package pl.mszarlinski.scala.dijkstra

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.PriorityQueueProxy

object Dijkstra extends App {
  /**
   * Returns total weight of shortest path from <tt>source</tt> to <tt>target</tt>.
   */
  def solution(from: Array[Int], to: Array[Int], weight: Array[Int], source: Int, target: Int): Int = {
    validateWeight(weight)

    val graph: MutableGraph = MutableGraph(from, to, weight, source, target)
    val queue: PriorityQueue[Node] = PriorityQueue() // queue with nodes ordered by weight

    queue += graph.source

    while (!queue.isEmpty) {
      processNode(queue.dequeue, graph, queue)
    }

    graph.target.weight

  }

  def processNode(node: Node, graph: MutableGraph, queue: PriorityQueue[Node]) = {
    for {
      set <- graph.nextNodes(node.index)
      u <- set
      e <- graph.getEdges(node.index, u.index)
    } {
      val newWeight = node.weight + e.weight
      if (u.weight == -1 || u.weight > newWeight) {
        u.weight = newWeight
        queue += u
      }
    }

  }

  def validateWeight(weight: Array[Int]) = {
    assert((weight.toList) forall (_ >= 0), "Weight cannot be negative")
  }

  println(solution(Array(0, 0, 1, 1, 2), Array(1, 2, 2, 3, 3), Array(1, 3, 1, 3, 1), 0, 3))
  // multiple edges
  println(solution(Array(0, 0, 1, 1, 2, 2), Array(1, 2, 2, 3, 3, 3), Array(1, 3, 1, 3, 1, 10), 0, 3))
  // negative weight
  println(solution(Array(0, 0, 1, 1, 2), Array(1, 2, 2, 3, 3), Array(1, 1, 1, 1, -1), 0, 3))
}