package edu.spbau.master.calculus.unificator.model

import edu.spbau.master.calculus.unificator.model.RichExpression._

import scala.language.implicitConversions

/**
  * @author Baidin Dima
  */
case class RichExpression(expression: Expression) {

  def contains(variable: Variable): Boolean = expression match {
    case v: Variable ⇒ v == variable
    case Function(_, args) ⇒ args.exists(_.contains(variable))
  }

  def replace(boundedVariables: Map[Variable, Expression]): Expression = expression match {
    case v: Variable ⇒ boundedVariables.getOrElse(v, v)
    case Function(name, args) ⇒ Function(name, args.map(_.replace(boundedVariables)))
  }

}


object RichExpression {
  implicit def toRichExpression(expression: Expression): RichExpression = {
    RichExpression(expression)
  }
}

object RichVariable {
  implicit def fromString(string: String): Variable = Variable(string)
}