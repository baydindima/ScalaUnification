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

  lazy val variables: Set[Variable] = (Set.empty[Variable] /: args) {
    case (s, expr) ⇒ expr match {
      case v: Variable ⇒ s + v
      case f: Function ⇒ s ++ f.variables
    }
  }

  def contains(variable: Variable): Boolean = variables.contains(variable)


  override def toString: String = args.mkString(s"$name(", ", ", ")")
}