package relation

import fivm.frontend.Type

class Variable(val name: String, val mtype: Type) {

  override def toString: String = name

  def canEqual(other: Any): Boolean = other.isInstanceOf[Variable]

  override def equals(other: Any): Boolean = other match {
    case that: Variable =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
