import fivm.frontend.SQLParser
import solvers.LPSolver
import utils.Utils
import variable_order.VariableOrder

object Main {

  System.loadLibrary("jniortools")

  def main(args: Array[String]): Unit = {
    val sqlSystem = SQLParser.apply(testSQL)

    val query = sqlSystem.queries.head

    val (relations, variables) = Utils.processQuery(query, sqlSystem)

    val constraints = Utils.readConstraintsFile("resources/constraints.txt", relations, variables)

    LPSolver.solve(variables, constraints)

    VariableOrder.compute(variables, relations, query)

//    println(constraints)
//    LPSolver.solve(variables, constraints, intLP = false)
  }

  val testSQL =
    """|CREATE TABLE R(A int, B int, C int)
       |FROM FILE 'experiments/data/r.dat' LINE DELIMITED
       |csv ();

       |CREATE TABLE S(A int, B int, D int)
       |FROM FILE 'experiments/data/s.dat' LINE DELIMITED
       |csv ();

       |CREATE TABLE T(A int, E int)
       |FROM FILE 'experiments/data/t.dat' LINE DELIMITED
       |csv ();

       |CREATE TABLE U(E int, F int)
       |FROM FILE 'experiments/data/t.dat' LINE DELIMITED
       |csv ();

       |SELECT * FROM R NATURAL JOIN S NATURAL JOIN T;""".stripMargin
}