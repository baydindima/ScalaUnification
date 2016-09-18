package edu.spbau.master.calculus.unificator.unification

import edu.spbau.master.calculus.unificator.model.{Expression, Function, Variable}
import edu.spbau.master.calculus.unificator.model.RichExpression._

import scala.util.{Failure, Success, Try}

/**
  * @author Baidin Dima
  */
object Unification {

  /**
    * Unify if possible expressions
    *
    * @param boundVariables cur bound variables
    * @param expressions    expressions for unification
    * @return updated bound variables if possible
    */
  def apply(boundVariables: Map[Variable, Expression],
            expressions: Expression*): Try[Map[Variable, Expression]] =
    expressions.toList match {
      case exp1 :: exp2 :: rest ⇒
        unify(exp1.replace(boundVariables),
          exp2.replace(boundVariables),
          boundVariables).flatMap { m ⇒
          rest match {
            case x :: xs ⇒ apply(m, x :: exp2 :: xs: _*)
            case _ ⇒ apply(m, exp2 :: rest: _*)
          }
        }
      case _ ⇒
        Try(buildAnswer(boundVariables))
    }

  /**
    * build DAG and update variables from leafs
    *
    * @param boundVariables cur bound variables
    * @return updated bound variables
    */
  private def buildAnswer(boundVariables: Map[Variable, Expression]): Map[Variable, Expression] = {
    var visitedVariables: Set[Variable] = Set.empty

    def updateVariable(boundVariables: Map[Variable, Expression],
                       parentVariable: Variable,
                       updatedVariable: Variable,
                       parents: Set[Variable]): Map[Variable, Expression] = {
      boundVariables.get(updatedVariable)
        .map { case e ⇒
          val result = dfs(boundVariables, updatedVariable, e, parents + parentVariable)
          result + (parentVariable → result(parentVariable).replace(result))
        }
        .getOrElse(boundVariables)
    }

    def dfs(boundVariables: Map[Variable, Expression],
            variable: Variable,
            expression: Expression,
            parents: Set[Variable]): Map[Variable, Expression] = {
      if (parents.contains(variable)) {
        throw new UnificationException(s"Cyclic dependency of ${variable.name} in $expression")
      }

      visitedVariables += variable

      expression match {
        case v: Variable ⇒
          updateVariable(boundVariables, parentVariable = variable, updatedVariable = v, parents)
        case f: Function ⇒
          (boundVariables /: f.variables) {
            case (m, v) ⇒
              updateVariable(m, parentVariable = variable, updatedVariable = v, parents)
          }
      }
    }

    (boundVariables /: boundVariables.keys) {
      case (m, v) ⇒
        if (visitedVariables.contains(v)) {
          m
        } else {
          dfs(m, v, m(v), Set.empty)
        }
    }
  }

  /**
    * Unify 2 expressions
    *
    * @param expr1          first expression for unification
    * @param expr2          second expression for unification
    * @param boundVariables cur bound variables
    * @return updated bound variables
    */
  private def unify(expr1: Expression,
                    expr2: Expression,
                    boundVariables: Map[Variable, Expression]): Try[Map[Variable, Expression]] = {
    def commonFailed(ex: UnificationException): Try[Map[Variable, Expression]] =
      Failure(UnificationException(s"Unification failed during processing of $expr1 and $expr2.", ex))

    (expr1, expr2) match {
      case _ if expr1 == expr2 ⇒ Success(boundVariables)
      case (v@Variable(_), e) ⇒
        unifyWithVariable(v, e, boundVariables).recoverWith {
          case ex: UnificationException ⇒ commonFailed(ex)
        }
      case (e, v@Variable(_)) ⇒
        unifyWithVariable(v, e, boundVariables).recoverWith {
          case ex: UnificationException ⇒
            commonFailed(ex)
        }
      case (Function(name1, args1), Function(name2, args2)) ⇒ name1 match {
        case _ if name1 != name2 ⇒
          commonFailed(new UnificationException(s"Different functions $name1 and $name2"))
        case _ if args1.length != args2.length ⇒
          commonFailed(new UnificationException(s"Functions $name1 has different arity"))
        case _ ⇒
          unifyArgs(args1.zip(args2).toList, boundVariables).recoverWith {
            case ex: UnificationException ⇒
              commonFailed(ex)
          }
      }
    }
  }

  /**
    * Bound variable with expression if possible
    *
    * @param variable       variable for bounding
    * @param expression     expression for bounding
    * @param boundVariables cur bound variables
    * @return updated bound variables
    */
  private def unifyWithVariable(variable: Variable,
                                expression: Expression,
                                boundVariables: Map[Variable, Expression]): Try[Map[Variable, Expression]] = {
    boundVariables.get(variable) match {
      case Some(bounding) ⇒ unify(bounding, expression, boundVariables)
      case None ⇒
        if (expression.contains(variable)) {
          Failure(new UnificationException(s"Cyclic dependency of ${variable.name} in $expression"))
        } else {
          Success(boundVariables + (variable → expression))
        }
    }
  }

  /**
    * Unify args of 2 functions
    *
    * @param args           pairs of args for unification
    * @param boundVariables cur bound variables
    * @return updated bound variables
    */
  private def unifyArgs(args: List[(Expression, Expression)],
                        boundVariables: Map[Variable, Expression]): Try[Map[Variable, Expression]] =
    args match {
      case Nil ⇒ Success(boundVariables)
      case (arg1, arg2) :: rest ⇒
        unify(arg1.replace(boundVariables),
          arg2.replace(boundVariables),
          boundVariables).flatMap { m ⇒
          unifyArgs(rest, m)
        }
    }

}
