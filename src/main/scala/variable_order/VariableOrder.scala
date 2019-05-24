package variable_order

import fivm.frontend.SQL.Query
import hypertree.Hypertree
import relation._

import scala.collection.mutable
import scala.util.control.Breaks._

object VariableOrder {

  private def computeWeights(variables: List[Variable], relations: List[Relation]): Map[Variable, Double] = {
    val weight = mutable.HashMap.empty[Variable, Double]

    for (v <- variables){
      var w = Double.MaxValue

      for (r <- relations){
        if (r.contains(v)){
          w = Math.min(w, r.size)
        }
      }

      weight.put(v, w)
    }

    weight.toMap
  }

  private def computeJoinVariables(relations: List[Relation], query: Query): Set[Variable] = {
    // TODO: get join variables from query
    // TODO: get join variables from query when joining of specific variables

    val variables = mutable.HashMap.empty[Variable, Int]

    for (r <- relations){
      for (v <- r.variables){
        variables.put(v, variables.getOrElse(v, 0) + 1)
      }
    }

    variables.filter(_._2 > 1).keySet.toSet
  }

  def areIndependent(u: Variable, v: Variable, relations: List[Relation]): Boolean = {
    for (r <- relations){
      if (r.contains(u) && r.contains(v))
        return false
    }

    true
  }

  def computeDependencies(node: VOTree, relations: List[Relation]): Unit ={
    var ancestor = node.parent

    while (ancestor != null){
      if (!areIndependent(node.variable, ancestor.variable, relations)){
        node.addDependency(ancestor.variable)
      }

      ancestor = ancestor.parent
    }

    println(node + " deps=" + node.getDependencies)

    node.getChildren.foreach(n => computeDependencies(n, relations))
  }

  def printVOTree(root: VOTree): Unit = {
    if (!root.isLeaf){
      for (c <- root.getChildren){
        println(root.variable + " --> " + c.variable)
      }

      for (c <- root.getChildren){
        printVOTree(c)
      }
    }
  }

  def buildHypertree(root: VOTree, hypertree: Hypertree): Unit ={
    def getVars(node: VOTree) = node.variable :: node.getDependencies

    hypertree.addNode(getVars(root))
    root.getChildren.foreach(n => buildHypertree(n, hypertree))

    for (c <- root.getChildren){
      hypertree.addEdge(hypertree.getNode(getVars(root)), hypertree.getNode(getVars(c)))
    }
  }

  def printHypergraph(root: VOTree): Unit = {
    def printHypernode(variable: Variable, deps: Iterable[Variable]): Unit ={
     val buffer = deps.toBuffer
      buffer += variable

      print(buffer.mkString("(", ",", ")"))
    }

    if (!root.isLeaf){
      for (c <- root.getChildren){
        printHypernode(root.variable, root.getDependencies)
        print(" --> ")
        printHypernode(c.variable, c.getDependencies)
        println()
      }

      for (c <- root.getChildren){
        printHypergraph(c)
      }
    }
  }

  def compute(variables: List[Variable], relations: List[Relation], query: Query): Unit = {
    println(variables)
    println(relations)

    val weight = computeWeights(variables, relations)
    val joinVariables = computeJoinVariables(relations, query)

    val sortedVariables = variables.sortWith((v1, v2) => {
      val isJoinV1 = joinVariables.contains(v1)
      val isJoinV2 = joinVariables.contains(v2)

      if (isJoinV1 && isJoinV2 || (!isJoinV1 && !isJoinV2))
        weight(v1) < weight(v2)
      else if (isJoinV1)
        true
      else
        false
    })

    println(weight)
    println(sortedVariables)

    val root = new VOTree(sortedVariables.head, null)
    println(root)

    for (v <- sortedVariables.tail){
      var node = root

      breakable {
        while (true) {
          if (node.isLeaf) {
            node.addChild(new VOTree(v, node))
            break()
          }

          var moved = false

          breakable {
            for (c <- node.getChildren) {
              if (!areIndependent(c.variable, v, relations)) {
                node = c
                moved = true
                break()
              }
            }
          }

          if (!moved) {
            node.addChild(new VOTree(v, node))
            break()
          }
        }
      }
    }

    println()
    printVOTree(root)
    computeDependencies(root, relations)
    println()
    printHypergraph(root)
    println()

    // TODO: fix hypertree implementation
    val hypertree = new Hypertree
    buildHypertree(root, hypertree)

    println(hypertree)
  }

}
