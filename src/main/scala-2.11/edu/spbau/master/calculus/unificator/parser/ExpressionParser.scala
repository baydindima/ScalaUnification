package edu.spbau.master.calculus.unificator.parser

import edu.spbau.master.calculus.unificator.model.{Expression, Function, Variable}

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * @author Baidin Dima
  */
object ExpressionParser extends RegexParsers {

  private def expression: Parser[Expression] = function | variable

  private def variable: Parser[Variable] =
    letterRegex ^^ {
      case name ⇒ Variable(name)
    }

  private def function: Parser[Function] =
    letterRegex ~ "(" ~ opt(args) ~ ")" ^^ {
      case name ~ "(" ~ maybeArgs ~ ")" ⇒
        val args = maybeArgs.getOrElse(Seq())
        Function(name, args)
    }

  private def args: Parser[Seq[Expression]] = expression ~ rep("," ~> expression) ^^ {
    case x ~ xs ⇒ x :: xs
  }

  private val letterRegex: Regex = """\w+""".r

  def apply(input: String): Try[Expression] = parseAll(expression, input) match {
    case Success(result, _) ⇒ scala.util.Success(result)
    case NoSuccess(msg, _) ⇒ scala.util.Failure(ParserException(input, msg))
  }


}
