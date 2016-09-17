package edu.spbau.master.calculus.unificator.unification

import edu.spbau.master.calculus.unificator.model.{Expression, Function, Variable}
import edu.spbau.master.calculus.unificator.model.RichExpression._

import scala.util.{Failure, Success, Try}

/**
  * @author Baidin Dima
  */
object Unification {

  def unifyExpressions(boundedVariables: Map[Variable, Expression],
                       expressions: Expression*): Try[Map[Variable, Expression]] =
    expressions.toList match {
      case exp1 :: exp2 :: rest ⇒
        unify(exp1.replace(boundedVariables),
          exp2.replace(boundedVariables),
          boundedVariables).flatMap { m ⇒
          rest match {
            case x :: xs ⇒ unifyExpressions(m, x :: exp2 :: xs: _*)
            case _ ⇒ unifyExpressions(m, exp2 :: rest: _*)
          }
        }
      case _ ⇒ Success(boundedVariables.mapValues(_.replace(boundedVariables)))
    }

  private def unifyWithVariable(variable: Variable,
                                expression: Expression,
                                boundedVariables: Map[Variable, Expression]): Try[Map[Variable, Expression]] = {
    boundedVariables.get(variable) match {
      case Some(bounding) ⇒ unify(bounding, expression, boundedVariables)
      case None ⇒
        if (expression.contains(variable)) {
          Failure(new UnificationException(s"Cyclic dependency of ${variable.name} in $expression"))
        } else {
          Success(boundedVariables + (variable → expression))
        }
    }
  }

  private def unifyArgs(args: List[(Expression, Expression)],
                        boundedVariables: Map[Variable, Expression]): Try[Map[Variable, Expression]] =
    args match {
      case Nil ⇒ Success(boundedVariables)
      case (arg1, arg2) :: rest ⇒
        unify(arg1.replace(boundedVariables),
          arg2.replace(boundedVariables),
          boundedVariables).flatMap { m ⇒
          unifyArgs(rest, m)
        }
    }

  private def unify(expr1: Expression,
                    expr2: Expression,
                    boundedVariables: Map[Variable, Expression]): Try[Map[Variable, Expression]] = {
    def commonFailed(ex: UnificationException): Try[Map[Variable, Expression]] =
      Failure(UnificationException(s"Unification failed during processing of $expr1 and $expr2.", ex))

    (expr1, expr2) match {
      case _ if expr1 == expr2 ⇒ Success(boundedVariables)
      case (v@Variable(_), e) ⇒
        unifyWithVariable(v, e, boundedVariables).recoverWith {
          case ex: UnificationException ⇒ commonFailed(ex)
        }
      case (e, v@Variable(_)) ⇒
        unifyWithVariable(v, e, boundedVariables).recoverWith {
          case ex: UnificationException ⇒
            commonFailed(ex)
        }
      case (Function(name1, args1), Function(name2, args2)) ⇒ name1 match {
        case _ if name1 != name2 ⇒
          commonFailed(new UnificationException(s"Different functions $name1 and $name2"))
        case _ if args1.length != args2.length ⇒
          commonFailed(new UnificationException(s"Functions $name1 has different arity"))
        case _ ⇒
          unifyArgs(args1.zip(args2).toList, boundedVariables).recoverWith {
            case ex: UnificationException ⇒
              commonFailed(ex)
          }
      }
    }
  }

}
