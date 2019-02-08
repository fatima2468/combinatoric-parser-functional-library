// Final Commit
package json

import scala.language.postfixOps
import org.scalatest.FlatSpec
import JSON._

class JSONSpec extends FlatSpec {

  /* Unit Test Cases */
  // JNull
  // Success
  "JNull" should "accept the string null as a success case" in {
    assert(JSON.parse("""null""") === JNull)
  }
  // Failure
  "JNull" should "throw an exception when the input doesn't match null" in {
    intercept[java.lang.Exception] {
      JSON.parse("""notnull""")
    }
  }

  // JBool
  // Success
  "JBool" should "accept the string true or false as a success case" in {
    // A boolean value is either a true or a false value
    assert(JSON.parse("""true""") === JBool(true))
    assert(JSON.parse("""false""") === JBool(false))
  }
  // Failure
  "JBool" should "throw an exception when the input doesn't match true or false" in {
    intercept[java.lang.Exception] {
      JSON.parse("""truee""")
      JSON.parse("""falseee""")
      JSON.parse("""randomstring""")
      JSON.parse("""false0""")
    }
  }


  // JString
  // Success
  "JString" should "accept a valid string defined for JSON" in {
    // A valid string in JSON is defined as a sequence of any unicode characters except " or \ or control character
    // and of escape characters written inside quotation marks
    // Simple string
    assert(JSON.parse(""""asimplestring"""") === JString("asimplestring")) // without whitespaces
    assert(JSON.parse(""""a simple string"""") === JString("a simple string")) // with whitespaces

    // Escape character test cases
    assert((JSON.parse("\"\\\"\"")) === JString("\\\"")) // quotation mark
    assert((JSON.parse("\"\\\\\"")) === JString("\\\\")) // reverse solidus
    assert((JSON.parse("\"/\"")) === JString("/")) // solidus
    assert(JSON.parse(""""\b\b"""") === JString("""\b\b""")) // backspace
    assert(JSON.parse(""""\n\n"""") === JString("""\n\n""")) // newline
    assert(JSON.parse(""""\f\f"""") === JString("""\f\f""")) // formfeed
    assert(JSON.parse(""""\t\t"""") === JString("""\t\t""")) // horizontal tab
    assert(JSON.parse(""""\r\r"""") === JString("""\r\r""")) // carriage return
    assert(JSON.parse(""""\u00af"""") === JString("""\u00af""")) // u<hexadecimal digit>
    assert(JSON.parse(""""\u00bd"""") === JString("""\u00bd""")) // u<hexadecimal digit>

    // Complex string
    assert(JSON.parse(""""abcdef\"abc\t\n\"/gdef"""") === JString("""abcdef\"abc\t\n\"/gdef"""))

  }
  // Failure
  "JString" should "throw an exception when the input string is an invalid JSON string" in {
    intercept[java.lang.Exception] {
      // String containing control characters
      JSON.parse("""abc\0""")
      JSON.parse("""abc\v""")
      JSON.parse("""randomstring\e""")
      JSON.parse("""random\"string\e""")
      JSON.parse("""random\\string\e""")
      JSON.parse("\"randomstring") // String without quote
    }
  }

  // JNumber
  // Success
  "JNumber" should "accept a valid JSON number" in {
    // A valid JSON number is a combination of digits, fractions, exponents and sign
    assert(JSON.parse("3") === JNumber(3))
    assert(JSON.parse("3.1") === JNumber(3.1))
    assert(JSON.parse("0.134") === JNumber(0.134))
    assert(JSON.parse("3.3e3") === JNumber(3300))
    assert(JSON.parse("3.3E3") === JNumber(3300))

    assert(JSON.parse("-3") === JNumber(-3))
    assert(JSON.parse("-3.1") === JNumber(-3.1))
    assert(JSON.parse("-0.134") === JNumber(-0.134))
    assert(JSON.parse("-3.3e3") === JNumber(-3300))
    assert(JSON.parse("-3.3E3") === JNumber(-3300))

    assert(JSON.parse("3.3e-3") === JNumber(0.0033))
    assert(JSON.parse("3.3E-3") === JNumber(0.0033))

  }
  // Failure
  "JNumber" should "throw an exception when the input is an invalid JSON number" in {
    intercept[java.lang.Exception] {
      JSON.parse("3.3a3")
      JSON.parse("a2,3")
    }
  }

  // JArray
  // Success
  "JArray" should "accept valid JSON arrays" in {
    // A valid JSON array is defined as an indexed sequence of JSON values written inside square brackets and
    // separated by comma. An array can contain all type of value and of variable nesting depths
    // Simple JArray
    assert(JSON.parse("[]") === JArray(Vector()))
    assert(JSON.parse("[1]") === JArray(Vector(JNumber(1.0))))
    assert(JSON.parse("[\"stringvalue\"]") === JArray(Vector(JString("stringvalue"))))
    assert(JSON.parse("[{}]") === JArray(Vector(JObject(Map()))))
    assert(JSON.parse("[null]") === JArray(Vector(JNull)))
    assert(JSON.parse("[true]") === JArray(Vector(JBool(true))))
    assert(JSON.parse("[false]") === JArray(Vector(JBool(false))))
    assert(JSON.parse("[[false]]") === JArray(Vector(JArray(Vector(JBool(false))))))

    //Complex JArray
    assert(JSON.parse("[true,false,null]") === JArray(Vector(JBool(true), JBool(false), JNull)))
    assert(JSON.parse("[true,false,null,{}]") === JArray(Vector(JBool(true), JBool(false), JNull, JObject(Map()))))
    assert(JSON.parse("[45.78,39.67,[4,3],5]") === JArray(Vector(JNumber(45.78), JNumber(39.67), JArray(Vector(JNumber(4.0), JNumber(3.0))), JNumber(5.0))))
    assert(JSON.parse("[45.78,39.67,[4,3],{\"x:Int=\":5}]") === JArray(Vector(JNumber(45.78), JNumber(39.67), JArray(Vector(JNumber(4.0), JNumber(3.0))), JObject(Map("x:Int=" -> JNumber(5.0))))))
    assert(JSON.parse("[null,[null,[{},{},true,\"stringvalue\"],false],43]") === JArray(Vector(JNull, JArray(Vector(JNull, JArray(Vector(JObject(Map()), JObject(Map()), JBool(true), JString("stringvalue"))), JBool(false))), JNumber(43.0))))
  }
  // Failure
  "JArray" should "throw an exception for invalid JArrays (be nested with in any JValues)" in {
    intercept[java.lang.Exception] {
      JSON.parse("[")
      JSON.parse("[,]")
      JSON.parse("[4,]")
      JSON.parse("[8,6}")
      JSON.parse("[[]")
      JSON.parse("[{},}]")

      JSON.parse("[null,[nul,[{},{},true,\"stringvalue\"],false],43]")
      JSON.parse("[null,[null,[{:},{},true,\"stringvalue\"],false],43]")
      JSON.parse("[null,[nul,[{},{,true,\"stringvalue\"],false],43]")
      JSON.parse("[null,[nul,[{},{},true,stringvalue\"],false],43]")
      JSON.parse("[null,[nul,[{},{},true,\"stringvalue\"],false],43.0a]")

    }
  }

  // JObject
  val object1 =
    """{
      |}
    """.stripMargin
  
  val expected1 = JObject (
    Map (
    )
  )

  val object2 =
    """{
      |  "array": [43, 42, 56]
      |}
    """.stripMargin
  
  val expected2 = JObject (
    Map(
      "array" -> JArray(Vector(JNumber(43.0), JNumber(42.0), JNumber(56.0)))
    )
  )

  val object3 =
    """{
      |   "nullvalue": null,
      |   "bool": true
      |}
    """.stripMargin
  
  val expected3 = JObject (
    Map (
      "nullvalue" -> JNull,
      "bool" -> JBool(true)
    )
  )

  val object4 =
    """{
      |  "array": [{"x": 3}, 42, null]
      |}
    """.stripMargin
  
  val expected4 = JObject(
    Map(
      "array" -> JArray(Vector(JObject(Map("x" -> JNumber(3.0))), JNumber(42.0), JNull))
    )
  )

  val object5 =
    """{
      |   "x": 3,
      |   "bool": true,
      |   "string": "Hello",
      |   "y": 5
      |}
    """.stripMargin
  
  val expected5 = JObject(
    Map(
      "x" -> JNumber(3.0),
      "bool" -> JBool(true),
      "string" -> JString("Hello"),
      "y" -> JNumber(5.0)
    )
  )

  // Success
  "JObject" should "accept valid JSON objects" in {
    assert(JSON.parse(object1) === expected1)
    assert(JSON.parse(object2) === expected2)
    assert(JSON.parse(object3) === expected3)
    assert(JSON.parse(object4) === expected4)
    assert(JSON.parse(object5) === expected5)
  }
  // Failure
  "JObject" should "throw an exception for invalid Objects (missing values, types, braces, etc)" in {
    intercept[java.lang.Exception] {
      JSON.parse("{")
      JSON.parse("{:}")
      JSON.parse("{\"array\":}")
      JSON.parse("{\"array\":[4, [5, [{}, {}, \"stringvalue\"], [{], true], false]}")
      JSON.parse("""{:null}""")
      JSON.parse("{{{:}}")
      JSON.parse("{\"string\":43}")
    }
  }

  val testInput =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello",
      |  "array": [{ "x": 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin

  val expected = JObject (
    Map (
      "number" -> JNumber(3.0),
      "bool" -> JBool(true),
      "string" -> JString("Hello"),
      "array" -> JArray(IndexedSeq(JObject(Map("x" -> JNumber(3))), JNumber(42), JNull)),
      "object" -> JObject(Map())
    )
  )

  "jsonParser" should "parse JSON data" in {
     assert(JSON.parse(testInput) === expected)
  }

  "parseWithExceptionCaught" should "catch an exception and print it if any otherwise return the successful result" in {
    // println statements are used to show the error instead of assert
    // assert is used where the similar cases are successful
    println(JSON.parseWithExceptionCaught("""notnull"""))
    assert(JSON.parseWithExceptionCaught("""null""") === JNull)
    println(JSON.parseWithExceptionCaught("""false0"""))
    assert(JSON.parseWithExceptionCaught("""false""") === JBool(false))
    println(JSON.parseWithExceptionCaught(""""abc\v""""))
    assert(JSON.parseWithExceptionCaught(""""abc"""") === JString("abc"))
    println(JSON.parseWithExceptionCaught("["))
    assert(JSON.parseWithExceptionCaught("[]") === JArray(Vector()))
    println(JSON.parseWithExceptionCaught("3.3a3"))
    assert(JSON.parseWithExceptionCaught("3.33") === JNumber(3.33))
    println(JSON.parseWithExceptionCaught("[null,[nul,[{},{},true,\"stringvalue\"],false],43]"))
    println(JSON.parseWithExceptionCaught("[null,[null,[{:},{},true,\"stringvalue\"],false],43]"))
    assert(JSON.parseWithExceptionCaught("[null,[null,[{},{},true,\"stringvalue\"],false],43]") ===
      JArray(Vector(JNull, JArray(Vector(JNull, JArray(Vector(JObject(Map()), JObject(Map()), JBool(true), JString("stringvalue"))), JBool(false))), JNumber(43.0))))
    println(JSON.parseWithExceptionCaught("{:}"))
    assert(JSON.parseWithExceptionCaught("{}") === JObject(Map()))
  }
}
