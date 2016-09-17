package edu.spbau.master.calculus.unificator.unification

/**
  * @author Baidin Dima
  */
class UnificationException(val cause: String) extends RuntimeException(cause)

object UnificationException {
  def apply(msg: String, cause: UnificationException): UnificationException =
    new UnificationException(msg + "\n" + cause.cause)
}
