package json

object JSON {

  
  import parsing._
  import Parser._
  sealed abstract class JSON

  case object JNull extends JSON
  case class JBool(b: Boolean) extends JSON
  case class JNumber(n: Double) extends JSON
  case class JString(s: String) extends JSON
  case class JArray(a: IndexedSeq[JSON]) extends JSON
  case class JObject(a: Map[String, JSON]) extends JSON
  case class JSyntaxError(string: String) extends JSON

  def parse(s: String): JSON = {
    //Space is avoided by Parser but for the test cases eliminating \n\r that are left after .stripMargin()
    jsonParser.parse(s.filterNot((x: Char) => x == '\n' || x == '\r').trim).get  
  }

  def list[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
    p andThen Parser.repeat(sep andThen p) map (a => (a._2.unzip._2).++:(List(a._1))) orElse Parser.succeed(Nil)

  def jsonParser: Parser[JSON] = new Parser[JSON] {

    def apply(loc: Location) = jValue.apply(loc)

    def jValue: Parser[JSON] = jObject orElse jArray orElse jNumber orElse jBool orElse jString  orElse jNull

    def jNull: Parser[JNull.type] = {
      tag("Failed to define jNull")(string("null").map (_ => JNull))
    }

    def jBool: Parser[JBool] = {
      tag("Failed to define jBool")(string("true").orElse(string("false")) map {
        case ("true") => JBool(true)
        case ("false") => JBool(false)
      })
    }

    def jNumber: Parser[JNumber] = {
      tag("Failed to define jNumber")(Parser.numbers map(x => JNumber(x)))
    }

    def jString: Parser[JString] = {
      tag("Failed to define jString")(string("\"") andThen(Parser.characters) andThen string("\"") map (a => JString(a._1._2)))
    }

    def jArray: Parser[JArray] = {
      tag("Failed to define jArray")(char('[') andThen list(jValue, char(',')) map (_._2)) andThen char(']') map {
        case (vs, _) => JArray(vs.toIndexedSeq)
      }
    }

    def jObject: Parser[JObject] = {
      tag("Failed to define jObject")((char('{') andThen list((jString andThen char(':') andThen jValue), char(',')) map (_._2)) andThen char('}') map (a => JObject((a._1.unzip._1.unzip._1.map(_.s).zip(a._1.unzip._2)).toMap) ))
    }
  }

  // Additional Method added for Error Reporting, returning a Syntax Error Message for the type of outermost JSON Object being parsed with pointing to error position.
  def parseWithExceptionCaught(s: String): JSON = {

    val x = jsonParser.parse(s)
    var errorMessage: String = ""
    if(x.isFailure) {
      x recover {
        case ex:Exception => {
          var _errorMessage = ex.getMessage
          errorMessage = ex.getMessage
          _errorMessage = _errorMessage.substring(_errorMessage.indexOf("Failed"))
          val obj = _errorMessage.substring(0, _errorMessage.indexOf("-")) + "for \n"
          val pos = _errorMessage.substring(_errorMessage.indexOf("position:"))
          val posVal = pos.substring(9).toInt
          errorMessage = obj + " " + s + "\n" +(" ".toString * posVal +  "^" + " (" + pos)
        }
      }
      (JSyntaxError(errorMessage))
    } else {
      x.get
    }

  }
  
}
