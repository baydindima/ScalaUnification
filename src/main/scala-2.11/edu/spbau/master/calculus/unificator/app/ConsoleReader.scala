package edu.spbau.master.calculus.unificator.app

import edu.spbau.master.calculus.unificator.model.{Expression, Variable}
import edu.spbau.master.calculus.unificator.parser.ExpressionParser

import scala.io.StdIn._
import scala.util.{Failure, Success, Try}

/**
  * @author Baidin Dima
  */
object ConsoleReader {

  def read(): Seq[Try[Expression]] = {
    val n = readInt()
    (1 to n).map(_ ⇒ readLine()).map(ExpressionParser.apply)
  }

  def printResult(result: Try[Map[Variable, Expression]]): Unit = {
    result match {
      case Failure(_) ⇒ println("None")
      case Success(map) ⇒
        println(map.size)
        map.foreach {
          case (v, exp) ⇒
            println(v.toString)
            println(exp)
        }
    }
  }

}
