package relation

class Relation(val name: String, val variables: List[Variable]) {

  var size = 0.0

  override def toString: String = name + variables.mkString("(", ",", ")")

  def contains(variable: Variable): Boolean = variables.contains(variable)

  def contains(variables: List[Variable]): Boolean = variables.forall(contains)
}

