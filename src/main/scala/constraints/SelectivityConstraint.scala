package constraints

import relation.{Relation, Variable}

import scala.collection.mutable.ArrayBuffer

class SelectivityConstraint(relation: Relation, value: Double) extends Constraint(relation, value) {

  override def toString = s"SelectivityConstraint($relation, $value)"
}

object SelectivityConstraint {

  def apply(expression: String, relations: List[Relation], variables: List[Variable]): SelectivityConstraint = {
    val tokens = expression.split("=")
    assert(tokens.length == 2)

    val expr = tokens(0)
    val p = tokens(1).toDouble

    val e = expr.substring("sel(".length, expr.length - 1)
    val elems = e.split(",")

    if (elems.size > 1){
      var newName = ""
      var newVariables = ArrayBuffer.empty[Variable]
      var productSizes = 1.0

      for (e <- elems){
        val optRelation = relations.find(r => r.name == e)

        if (optRelation.nonEmpty){
          if (newName != "")
            newName += "⋈"

          newName += optRelation.get.name
          newVariables ++= optRelation.get.variables
          productSizes *= optRelation.get.size
        }
      }

      if (newName.nonEmpty) {
        return new SelectivityConstraint(new Relation(newName, newVariables.toSet.toList), p * productSizes)
      }
    }

    throw new IllegalArgumentException(expression)
  }
}
