package edu.spbau.master.calculus.unificator.model

/**
  * @author Baidin Dima
  */
sealed trait Expression

case class Variable(name: String) extends Expression {
  require(name != null, "Name is null!")

  override def toString: String = name
}

case class Function(name: String, args: Seq[Expression]) extends Expression {
  require(name != null, "Name is null!")
  require(args != null, "Args is null!")

  def contains(variable: Variable): Boolean = (false /: args) {
    case (result, expr) ⇒ result || {
      expr match {
        case v: Variable ⇒ v == variable
        case f: Function ⇒ f.contains(variable)
      }
    }
  }

  override def toString: String = args.mkString(s"$name(", ", ", ")")
}