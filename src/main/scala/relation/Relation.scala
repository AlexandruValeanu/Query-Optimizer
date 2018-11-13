package relation

class Relation(val name: String, val variables: List[Variable]) {

  override def toString: String = name + variables.mkString("(", ",", ")")
}