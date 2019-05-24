package variable_order

import relation.Variable

import scala.collection.mutable.ArrayBuffer

class VOTree(val variable: Variable, val parent: VOTree) {

  private val children = ArrayBuffer.empty[VOTree]
  private val deps = ArrayBuffer.empty[Variable]

  def addChild(node: VOTree): Unit = children += node

  def addDependency(variable: Variable): Unit = deps += variable

  // TODO: improve efficiency
  def getChildren = children.toList

  // TODO: improve efficiency
  def getDependencies = deps.toList

  def isLeaf: Boolean = children.isEmpty

  override def toString = s"VOTree($variable)"
}
