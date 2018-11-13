import fivm.frontend.SQLParser
import solvers.LPSolver
import utils.Utils

object Main {

  System.loadLibrary("jniortools")

  def main(args: Array[String]): Unit = {
    val sqlSystem = SQLParser.apply(testSQL)

    val query = sqlSystem.queries.head

    val (relations, variables) = Utils.processQuery(query, sqlSystem)

    println(relations)
    println(variables)

    val constraints = Utils.readConstraintsFile("resources/constraints.txt", relations, variables)

    println(constraints)

    LPSolver.solve(variables, constraints, intLP = false)
  }

  val testSQL =
    """|CREATE TABLE R(A int, B int)
       |FROM FILE 'experiments/data/r.dat' LINE DELIMITED
       |csv ();

       |CREATE TABLE S(B int, C int)
       |FROM FILE 'experiments/data/s.dat' LINE DELIMITED
       |csv ();

       |CREATE TABLE T(C int, D int)
       |FROM FILE 'experiments/data/t.dat' LINE DELIMITED
       |csv ();

       |CREATE TABLE U(C int, D int)
       |FROM FILE 'experiments/data/t.dat' LINE DELIMITED
       |csv ();

       |SELECT * FROM R NATURAL JOIN S;""".stripMargin
}