package hypertree

import hypertree.Hypertree.Node
import relation.Variable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Hypertree {

  private val nodes = mutable.HashMap.empty[List[Variable], Node]
  private val edges = mutable.HashMap.empty[Node, ArrayBuffer[Node]]

  def addNode(variables: List[Variable]): Unit ={
    addNode(getNode(variables))
  }

  def addNode(node: Node): Unit ={
    edges.put(node, ArrayBuffer.empty)
  }

  def addEdge(u: Node, v: Node): Unit ={
    edges(u) += v
  }

  def getNode(variables: List[Variable]): Node ={
    if (!nodes.contains(variables)){
      nodes.put(variables, new Node(variables))
      nodes(variables)
    }

    nodes(variables)
  }


  override def toString = s"Hypertree(${nodes.keySet}, $edges)"
}

object Hypertree {
  class Node(val variables: List[Variable]){

    def canEqual(other: Any): Boolean = other.isInstanceOf[Hypertree]

    override def equals(other: Any): Boolean = other match {
      case that: Node =>
        (that canEqual this) &&
          variables.toSet == that.variables.toSet
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(variables)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString = s"Node(${variables.mkString(",")})"
  }
}
