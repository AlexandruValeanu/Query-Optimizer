package examples

import com.google.ortools.linearsolver.MPSolver
// Copyright 2010-2017 Google
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.


/**
  * Integer programming example that shows how to use the API.
  *
  */
object IntegerProgramming {
  private def createSolver(solverType: String) = try
    new MPSolver("IntegerProgrammingExample", MPSolver.OptimizationProblemType.valueOf(solverType))
  catch {
    case e: IllegalArgumentException =>
      null
  }

  private def runIntegerProgrammingExample(solverType: String): Unit = {
    val solver = createSolver(solverType)
    if (solver == null) {
      System.out.println("Could not create solver " + solverType)
      return
    }
    val infinity = MPSolver.infinity
    // x1 and x2 are integer non-negative variables.
    val x1 = solver.makeIntVar(0.0, infinity, "x1")
    val x2 = solver.makeIntVar(0.0, infinity, "x2")
    // Minimize x1 + 2 * x2.
    val objective = solver.objective()
    objective.setCoefficient(x1, 1)
    objective.setCoefficient(x2, 2)
    // 2 * x2 + 3 * x1 >= 17.
    val ct = solver.makeConstraint(17, infinity)
    ct.setCoefficient(x1, 3)
    ct.setCoefficient(x2, 2)
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
    System.out.println("Problem solved in " + solver.wallTime + " milliseconds")
    // The objective value of the solution.
    System.out.println("Optimal objective value = " + solver.objective.value)
    // The value of each variable in the solution.
    System.out.println("x1 = " + x1.solutionValue)
    System.out.println("x2 = " + x2.solutionValue)
    System.out.println("Advanced usage:")
    System.out.println("Problem solved in " + solver.nodes + " branch-and-bound nodes")
  }

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    System.out.println("---- Integer programming example with SCIP (recommended) ----")
    runIntegerProgrammingExample("SCIP_MIXED_INTEGER_PROGRAMMING")
    System.out.println("---- Integer programming example with CBC ----")
    runIntegerProgrammingExample("CBC_MIXED_INTEGER_PROGRAMMING")
    System.out.println("---- Integer programming example with GLPK ----")
    runIntegerProgrammingExample("GLPK_MIXED_INTEGER_PROGRAMMING")
  }

  try System.loadLibrary("jniortools")

}
