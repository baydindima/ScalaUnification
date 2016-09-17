package edu.spbau.master.calculus.unificator.unification

import edu.spbau.master.calculus.unificator.model.RichVariable.fromString
import edu.spbau.master.calculus.unificator.model.{Function => F, Variable => V}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
  * @author Baidin Dima
  */
@RunWith(classOf[JUnitRunner])
class UnificationSpec extends FlatSpec with Matchers {

  it should "unify as examples" in {
    Unification.unifyExpressions(Map.empty, "a", "b") shouldEqual Success(Map(V("a") → V("b")))

    Unification.unifyExpressions(Map.empty,
      F("f", Seq("a", "b", "c")),
      F("f", Seq(F("g", Seq(V("b"))), V("b"), V("c")))) shouldEqual Success(Map(V("a") → F("g", Seq(V("b")))))

    Unification.unifyExpressions(Map.empty,
      "a",
      "b",
      V("c")) shouldEqual Success(Map(V("a") → V("b"), V("c") → V("b")))

    Unification.unifyExpressions(Map.empty,
      V("a"),
      F("f", Seq("x", "y")),
      F("f", Seq(F("f", Seq("b")), F("g", Seq("x"))))
    ) shouldEqual Success(Map(
      V("a") → F("f", Seq(F("f", Seq(V("b"))), F("g", Seq(F("f", Seq(V("b"))))))),
      V("x") → F("f", Seq(V("b"))),
      V("y") → F("g", Seq(F("f", Seq(V("b"))))))
    )

    Unification.unifyExpressions(Map.empty,
      F("f", Seq("x", "y")),
      F("f", Seq("y", "x"))) shouldEqual Success(Map(
      V("x") → V("y"))
    )

    Unification.unifyExpressions(Map.empty,
      F("f", Seq("x", F("f", Seq("x")))),
      F("f", Seq("x", "y"))) shouldEqual Success(Map(
      V("y") → F("f", Seq(V("x"))))
    )
  }

  it should " not unify as examples" in {
    the[UnificationException] thrownBy Unification.unifyExpressions(
      Map.empty,
      F("f",Seq("a", "b")),
      F("f",Seq(F("g",Seq("a")), "c")),
      F("f",Seq("a", "b"))).get

    the[UnificationException] thrownBy Unification.unifyExpressions(
      Map.empty,
      F("f",Seq("x")),
      F("g",Seq("y"))).get

    the[UnificationException] thrownBy Unification.unifyExpressions(
      Map.empty,
      F("f",Seq("x", F("f",Seq("x")))),
      F("f",Seq("y"))).get
  }

}
