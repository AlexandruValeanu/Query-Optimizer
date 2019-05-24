package constraints

import expression.ArithmeticParser
import relation.{Relation, Variable}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Exception.allCatch


class SizeConstraint(relation: Relation, value: Double) extends Constraint(relation, value) {

  override def toString = s"SizeConstraint($relation, $value)"
}

object SizeConstraint {

  def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  def apply(expression: String, relations: List[Relation], variables: List[Variable]): SizeConstraint = {
    val tokens = expression.split("=")
    assert(tokens.length == 2)

    val expr = tokens(0)
    var size = 0.0

    if (isDoubleNumber(tokens(1))){
      size = tokens(1).toDouble
    }
    else{
      val values = Map.empty[String, Double].updated("n", Math.E)

      size = ArithmeticParser.eval(tokens(1), values).get
    }

    val e = expr.substring("size(".length, expr.length - 1)
    val elems = e.split(",")

    if (elems.size == 1){
      val optRelation = relations.find(r => r.name == elems(0))

      if (optRelation.nonEmpty){
        // set mutable size field of the relation
        optRelation.get.size = size
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
          if (newName != "")
            newName += "∪"

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
