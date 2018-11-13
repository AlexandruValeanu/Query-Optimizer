package solvers

import com.google.ortools.linearsolver.MPSolver
import constraints.SizeConstraint
import relation.Variable

object LPSolver {

  def solve(variables: List[Variable], constraints: List[SizeConstraint], intLP: Boolean = false): Unit = {
    val solverType =
      if (intLP){
        "SCIP_MIXED_INTEGER_PROGRAMMING"
      }
      else{
        "GLOP_LINEAR_PROGRAMMING"
      }

    val solver = new MPSolver(solverType, MPSolver.OptimizationProblemType.valueOf(solverType))

    if (solver == null) {
      println("Could not create solver " + solverType)
      return
    }

    val infinity = MPSolver.infinity

    // weights are continuous non-negative variables (one for each relation)
    val weightMap = constraints.map(r => (r,
      if (intLP) {
        solver.makeIntVar(0.0, 1.0, "w_" + r.relation.name)
      }
      else{
        solver.makeNumVar(0.0, 1.0, "w_" + r.relation.name)
      }
    )).toMap

    val objective = solver.objective
    objective.setMinimization()

    for (constraint <- constraints){
      objective.setCoefficient(weightMap(constraint), Math.log(constraint.size))
    }

    for (variable <- variables){
      val c = solver.makeConstraint(1, infinity, "constraint_" + variable.name)

      for ((constraint, weight) <- weightMap){
        if (constraint.relation.variables.contains(variable)){
          c.setCoefficient(weight, 1)
        }
      }
    }

    println("Number of variables = " + solver.numVariables)
    println("Number of constraints = " + solver.numConstraints)

    if (true) {
      val model = solver.exportModelAsLpFormat(false)
      println(model)
    }

    val resultStatus = solver.solve

    // Check that the problem has an optimal solution.
    if (resultStatus ne MPSolver.ResultStatus.OPTIMAL) {
      System.err.println("The problem does not have an optimal solution!")
      return
    }

    // Verify that the solution satisfies all constraints (when using solvers
    // others than GLOP_LINEAR_PROGRAMMING, this is highly recommended!).
    if (!solver.verifySolution(/*tolerance=*/ 1e-7, /*logErrors=*/ true)) {
      System.err.println("The solution returned by the solver violated the" + " problem constraints by at least 1e-7")
      return
    }

    // The objective value of the solution.
    println("Optimal objective value = " + solver.objective.value)

    // The value of each variable in the solution.
    for (w <- weightMap.values){
      println(w.name() + " = " + w.solutionValue())
    }
  }

}
