package relation

class Relation(val name: String, val variables: List[Variable]) {

  var size = 0

  override def toString: String = name + variables.mkString("(", ",", ")")
}
