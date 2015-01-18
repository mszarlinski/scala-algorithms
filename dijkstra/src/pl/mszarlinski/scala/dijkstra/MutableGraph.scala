package pl.mszarlinski.scala.dijkstra

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

object MutableGraph {
  /**
   * Builds graph from given parameters
   */
  def apply(from: Array[Int], to: Array[Int], weight: Array[Int], source: Int, target: Int): MutableGraph = {
    val graph = createEmptyGraph(source, target)
    for (i <- 0 to from.length - 1) {
      graph.addEdge(new Edge(from(i), to(i), weight(i)))
    }
    graph.source.weight = 0
    graph
  }

  def createEmptyGraph(source: Int, target: Int): MutableGraph = {
    val sourceNode = new Node(source)
    val targetNode = new Node(target)
    new MutableGraph(sourceNode, targetNode)
  }
}

class MutableGraph(val source: Node, val target: Node) {

  val neighboursMap: HashMap[Int, Set[Node]] = HashMap()
  val edgesMap: HashMap[(Int, Int), Set[Edge]] = HashMap()

  def addEdge(e: Edge) = {
    updateEdgesMap(e)
    updateNeighboursMap(e)
  }

  private def updateEdgesMap(e: Edge) = {
    val key = (e.from, e.to)
    if (!(edgesMap contains key)) {
      val edgesSet: Set[Edge] = new HashSet
      edgesMap put (key, edgesSet)
    }

    (edgesMap get key) match { case Some(s) => s += e }
  }

  private def updateNeighboursMap(e: Edge) = {
    if (!(neighboursMap contains e.from)) {
      val toSet: Set[Node] = new HashSet
      neighboursMap put (e.from, toSet)
    }

    val toNode: Node = if (e.to == target.index) target else new Node(e.to)
    neighboursMap get e.from match { case Some(s) => s += toNode }
  }

  def nextNodes(n: Int): Option[Set[Node]] = neighboursMap get n
  def getEdges(from: Int, to: Int): Set[Edge] = edgesMap get (from, to) match {
    case Some(s) => s
    case None => throw new IllegalArgumentException("Edges not found: ${from} -> ${to}")
  }
}

class Edge(val from: Int, val to: Int, val weight: Int) {
}

class Node(val index: Int, var weight: Int = -1) extends Ordered[Node] {
  def compare(that: Node) = that.weight - weight
}