package arithmeticExpParser

import org.scalatest.FlatSpec
import scala.language.postfixOps

class ArithExpParseCalcSpec extends FlatSpec {


  "ArithmeticExpressionParser" should "parse all valid arithmetic expressions and evaluate it with showing the result (Only one test case i.e. for negative numbers(-2) is not added))" in {
  assert(ArithExpParseCalc.parse("(67)") === 67.0)
  assert(ArithExpParseCalc.parse("67.98") === 67.98)
  assert(ArithExpParseCalc.parse("(67+2-2+3/10.0)") === 67.3)
  assert(ArithExpParseCalc.parse("((67+2-2+3)/10.0)") === 7.0)
  assert(ArithExpParseCalc.parse("(67+2-2+3)/10.0") === 7.0)
  assert(ArithExpParseCalc.parse("(((67+2-2+3)/10.0)+5)") === 12.0)
  assert(ArithExpParseCalc.parse("(((67+2-2+3)/10.0)x(5+4))") == 63.0)
  assert(ArithExpParseCalc.parse("3x60") == 180.0)   //Out of time, had to use character 'x' for multiplication identification
}

  "ArithmeticExpressionParser" should "throw an exception upon invalid input other arithmetic expressions" in {
  intercept[java.lang.Exception] {
  ArithExpParseCalc.parse("(67,)")
  ArithExpParseCalc.parse("67.9@8")
  ArithExpParseCalc.parse("(67+2-2,+3/10.0)")
  ArithExpParseCalc.parse("((67+2-2+((3)/10.0)")
  ArithExpParseCalc.parse("(67+2-2+3))/10.0")
  ArithExpParseCalc.parse("(((67+2-2+3-)/10.0)+5)")
  ArithExpParseCalc.parse("(((67+2-2+3)/10.0)x(5+4)+)")
  ArithExpParseCalc.parse("3x6a0") //Out of time, had to use character 'x' for multiplication identification
}
}
}