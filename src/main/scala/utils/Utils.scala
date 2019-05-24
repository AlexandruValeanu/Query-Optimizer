package utils

import constraints.{Constraint, SelectivityConstraint, SizeConstraint}
import fivm.frontend.SQL
import fivm.frontend.SQL.{TableJoin, TableNamed}
import relation.{Relation, Variable}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Utils {

  def processQuery(query: SQL.Query, sqlSystem: SQL.System): (List[Relation], List[Variable]) = query match {
      case q: SQL.Select =>
        val relations = ArrayBuffer.empty[Relation]
        val variables = ArrayBuffer.empty[Variable]

        val allRelations = sqlSystem.sources.map(x => {
          val name = x.schema.name
          val _variables = x.schema.fields.map(v => {
            val variable = new Variable(v._1, v._2)
            variables.find(x => x == variable) match {
              case Some(oldVar) => oldVar
              case None =>
                variables += variable
                variable
            }
          })

          new Relation(name, _variables)
        })

        def computeTables(table: SQL.Table) : Unit  = table match {
          case tableNamed: TableNamed =>
            relations += allRelations.find(x => x.name == tableNamed.n).get
          case tableJoin: TableJoin =>
            computeTables(tableJoin.t1)
            computeTables(tableJoin.t2)
          case _ => throw new IllegalArgumentException(table + " " + table.getClass)
        }

        for (t <- q.ts)
          computeTables(t)

        (relations.toList, relations.flatMap(x => x.variables).toSet.toList)
      case _ => throw new IllegalArgumentException(query + " " + query.getClass)
  }

  def getRelations(query: SQL.Query, sqlSystem: SQL.System): List[Relation] =
    processQuery(query, sqlSystem)._1

  def getVariables(query: SQL.Query, sqlSystem: SQL.System): List[Variable] =
    processQuery(query, sqlSystem)._2

  def readConstraintsFile(pathToFIle: String, relations: List[Relation], variables: List[Variable]): List[Constraint] = {
    val filename = pathToFIle
    val constraints = ArrayBuffer.empty[Constraint]

    for (line <- Source.fromFile(filename).getLines) {
      if (!line.startsWith("--") && line.length > 0){
        val tokens = line.split("=")
        assert(tokens.length == 2)

        if (tokens(0).startsWith("size")){
          constraints += SizeConstraint.apply(line, relations, variables)
        }
        else if (tokens(0).startsWith("sel")){
          constraints += SelectivityConstraint.apply(line, relations, variables)
        }
      }
    }

    constraints.toList
  }
}
