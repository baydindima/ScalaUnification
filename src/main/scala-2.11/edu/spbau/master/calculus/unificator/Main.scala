package edu.spbau.master.calculus.unificator

import edu.spbau.master.calculus.unificator.app.ConsoleReader
import edu.spbau.master.calculus.unificator.unification.Unification

import scala.util.Failure


/**
  * @author Baidin Dima
  */
object Main extends App {

  ConsoleReader.read() match {
    case expressions ⇒
      ConsoleReader.printResult(expressions.find(_.isFailure) match {
        case Some(Failure(ex)) ⇒ Failure(ex)
        case None ⇒
          Unification(Map.empty,
            expressions.map(_.get): _*)
        case _ ⇒ Failure(new RuntimeException("Can't happen"))
      })
  }

}
