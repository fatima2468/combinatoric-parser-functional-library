package arithmeticExpParser

object ArithExpParseCalc {
  import parsing._


  sealed abstract class ArithExpParseCalc {
    def evaluate(): Double
  }

  case class Other(op: String, v: ArithExpParseCalc)
  case class Number(n: Double) extends ArithExpParseCalc { def evaluate(): Double = n }
  case class Expression(op: String, a: ArithExpParseCalc, b: ArithExpParseCalc) extends ArithExpParseCalc {
    def evaluate(): Double = op match {
      case "x" => a.evaluate() * b.evaluate()
      case "/" => a.evaluate() / b.evaluate()
      case "+" => a.evaluate() + b.evaluate()
      case "-" => a.evaluate() - b.evaluate()
    }
  }
  case class ExpressionTree(v: ArithExpParseCalc, list: List[Other]) extends ArithExpParseCalc {
    def evaluate(): Double = list.foldLeft(v.evaluate()) {
      (r: Double, list: Other) => Expression(list.op, Number(r), list.v).evaluate()
    }
  }
  case class Parenthesis(e: ArithExpParseCalc) extends ArithExpParseCalc {
    def evaluate(): Double = e.evaluate()
  }

  def parse(s: String): Double = arithmeticExpParser.parse(s).get.evaluate()

  def arithmeticExpParser: Parser[ArithExpParseCalc] = new Parser[ArithExpParseCalc] {

    // Followed the idea of defining Context Free Grammers of Arithmatic Expressions Parsers
    // Expr -> Expr + Term | Expr - Term | Term
    // Term -> Term * Factor | Term / Factor | Factor
    // Factor -> (Expr) | Number

    import Parser._

    def apply(loc: Location) = expr.apply(loc)

    def multiplicationDivision: Parser[Other] = ("x" | "\\/") ~ factor map (a => Other(a._1, a._2)) //Multiplication and Division having same precedence

    def additionSubtraction: Parser[Other] = ("\\+" | "\\-") ~ term map (a => Other(a._1, a._2))  //Addition and Subtraction having same precedence

    // Expr -> Expr + Term | Expr - Term | Term
    def expr: Parser[ArithExpParseCalc] = term ~ repeat(additionSubtraction) map { a => ExpressionTree(a._1, a._2) }

    // Term -> Term * Factor | Term / Factor | Factor
    def term: Parser[ArithExpParseCalc] = factor ~ repeat(multiplicationDivision) map { a => ExpressionTree(a._1, a._2) }

    // Factor -> (Expr) | Number
    def factor: Parser[ArithExpParseCalc] = (numbers  map({ case n => Number((n))} )) orElse ('(' ~> expr <~ ')' map { case e => Parenthesis(e)})

  }
}


