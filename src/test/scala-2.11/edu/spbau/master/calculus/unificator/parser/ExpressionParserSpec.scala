package edu.spbau.master.calculus.unificator.parser

import edu.spbau.master.calculus.unificator.model.{Variable ⇒ V, Function ⇒ F}
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import edu.spbau.master.calculus.unificator.model.RichVariable._

import scala.util.{Success ⇒ S}

/**
  * @author Baidin Dima
  */
@RunWith(classOf[JUnitRunner])
class ExpressionParserSpec extends FlatSpec with Matchers {

  it should "parse single variables" in {
    ExpressionParser("a") shouldEqual S(V("a"))
    ExpressionParser("abc") shouldEqual S(V("abc"))
    ExpressionParser("ABC") shouldEqual S(V("ABC"))
  }

  it should "parse function without args" in {
    ExpressionParser("f()") shouldEqual S(F("f", Seq.empty))
    ExpressionParser("foo()") shouldEqual S(F("foo", Seq.empty))
    ExpressionParser("BAR()") shouldEqual S(F("BAR", Seq.empty))
  }

  it should "parse function with variable as args" in {
    ExpressionParser("f(a, b, c)") shouldEqual S(F("f", Seq("a", "b", "c")))
    ExpressionParser("foo(a,b,c)") shouldEqual S(F("foo", Seq("a", "b", "c")))
    ExpressionParser("BAR(aB,bcF,Ec)") shouldEqual S(F("BAR", Seq("aB", "bcF", "Ec")))
  }

  it should "parse function with function as args" in {
    ExpressionParser("f(g(h(q)))") shouldEqual S(F("f", Seq(F("g", Seq(F("h", Seq("q")))))))
  }

  it should "throw exception if input is invalid" in {
    the[ParserException] thrownBy ExpressionParser("()").get
    the[ParserException] thrownBy ExpressionParser("a b").get
    the[ParserException] thrownBy ExpressionParser("f() g()").get
    the[ParserException] thrownBy ExpressionParser("f(a, b,)").get
    the[ParserException] thrownBy ExpressionParser("f(a, b,").get
    the[ParserException] thrownBy ExpressionParser("f (a, b) c").get
    the[ParserException] thrownBy ExpressionParser("f(a, b, c").get
    the[ParserException] thrownBy ExpressionParser("f(a, (), c)").get
  }
}
