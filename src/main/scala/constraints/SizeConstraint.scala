package constraints

import relation.{Relation, Variable}

import scala.collection.mutable.ArrayBuffer

class SizeConstraint(val relation: Relation, var size: Int) {

  override def toString = s"SizeConstraint($relation, $size)"
}

object SizeConstraint {

  def apply(expression: String, relations: List[Relation], variables: List[Variable]): SizeConstraint = {
    val tokens = expression.split("=")
    assert(tokens.length == 2)

    val expr = tokens(0)
    val size = tokens(1).toInt

    val e = expr.substring(1, expr.length - 1)
    val elems = e.split(",")

    if (elems.size == 1){
      val optRelation = relations.find(r => r.name == elems(0))

      if (optRelation.nonEmpty){
        return new SizeConstraint(optRelation.get, size)
      }
      else{
        val optVariable = variables.find(v => v.name == elems(0))

        if (optVariable.nonEmpty){
          val variable = optVariable.get
          return new SizeConstraint(new Relation(variable.name, List(variable)), size)
        }
      }
    }
    else{
      var newName = ""
      var newVariables = ArrayBuffer.empty[Variable]

      for (e <- elems){
        val optVariable = variables.find(v => v.name == e)

        if (optVariable.nonEmpty){
          newName += optVariable.get.name
          newVariables += optVariable.get
        }
      }

      if (newName.nonEmpty) {
        return new SizeConstraint(new Relation(newName, newVariables.toList), size)
      }
    }

    throw new IllegalArgumentException(expression)
  }
}
