package expression

object ArithmeticParser extends scala.util.parsing.combinator.RegexParsers {

  private var values: Map[String, Double] = _

  def eval(expression: String) : Option[Double] = {
    values = Map.empty[String, Double]

    parseAll(expr, expression) match {
      case Success(result, _) =>
        Some(result())
      case _ =>
        None
    }
  }

  def eval(expression: String, values: Map[String, Double]) : Option[Double] = {
    this.values = values

    parseAll(expr, expression) match {
      case Success(result, _) =>
        Some(result())
      case _ =>
        None
    }
  }

  private def expr : Parser[() => Double] = {
    (term<~"+")~expr ^^ { case l~r => () => l() + r() } |
      (term<~"-")~expr ^^ { case l~r => () => l() - r() } |
      term
  }

  private def term : Parser[() => Double] = {
    (term2<~"*")~term ^^ { case l~r => () => l() * r() } |
      (term2<~"/")~term ^^ { case l~r => () => l() / r() } |
      term2
  }

  private def term2 : Parser[() => Double] = {
    (factor<~"^")~term2 ^^ { case l~r => () => Math.pow(l(), r()) } |
      factor
  }

  private def factor : Parser[() => Double] = {
    "("~>expr<~")" |
      "\\d+".r ^^ { x => () => x.toDouble } |
      "\\w+".r ^^ { x => () => values(x) } |
      failure("expected a value")
  }
}