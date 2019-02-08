//Final Commit

package parsing

import scala.language.postfixOps
import org.scalatest.FlatSpec

import scala.util.Failure

class ParserSpec extends FlatSpec {

  import parsing.Parser._

  // Single character test cases
  // Success
  "Single character Test Cases" should "process a single character and Fail is character doesn't match" in {
    assert('a'.parse('a').get === 'a')
    assert('a'.parse('a'.toString).get === 'a')
  }
  // Failure
  "Single character Test Cases" should "throw exception for failure cases" in {
    intercept[java.lang.Exception] {
      'b'.parse('a').get
      'a'.parse('b').get
    }
  }

  // String test cases
  // Success
  "String Test Cases" should "process a string and fail in scenarios described in comments" in {
    assert("a".parse("a").get === "a")
    assert("a".parse('a').get === "a")
    assert("abc".parse("abc").get === "abc")
    assert("This is a string".r.parse("This is a string").get === "This is a string")
  }
  // Failure
  "String Test Cases" should "throw an exception when input cannot be parsed" in {
    intercept[java.lang.Exception] {
      "a".parse("b").get
      "abc".parse("ab").get
      "abc".parse("bc").get
      "abc".parse("a").get
      "abc".parse("abcd").get
      "abc".parse("bcd").get
      "abc".parse("aac").get
      "abc".parse("aba").get
    }
  }

  // orElse
  // Success
  "orElse Test Cases" should "process p1 and p2 and succeeds if either p1 or p2 succeeds in uncommitted state" in {
    assert(('a' orElse 'b').parse('a').get === 'a')
    assert(('a' orElse 'b').parse('b').get === 'b')
    assert((string("a") orElse string("b")).parse("a").get === "a")
    assert((string("a") orElse string("b")).parse("b").get === "b")
    assert((string("ab") orElse string("cd")).parse("ab").get === "ab")
    assert((string("ab") orElse string("cd")).parse("cd").get === "cd")
    assert((string("ab") orElse string("aa")).parse("aa").get === "aa")
    // Multiple orElse
    assert((string("ab") orElse (string("cd") orElse string("ef")) orElse string("gh")).parse("ab").get === "ab")
    assert((string("ab") orElse (string("cd") orElse string("ef")) orElse string("gh")).parse("cd").get === "cd")
    assert((string("ab") orElse (string("cd") orElse string("ef")) orElse string("gh")).parse("ef").get === "ef")
    assert((string("ab") orElse (string("cd") orElse string("ef")) orElse string("gh")).parse("gh").get === "gh")
  }
  // Failure
  "orElse Test Cases" should "throw an exception if p1 and p2 both fail or either one failes in committed state" in {
    intercept[java.lang.Exception] {
      (('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa").get
      (string("ab") orElse string("cd")).parse("ef").get
      (string("ab") orElse (string("cd") orElse string("ef")) orElse string("gh")).parse("ij").get
    }
  }

  // andThen
  // Success
  "andThen Test Cases" should "process p1 and p2 and fail in scenarios where either one fails" in {
    assert(('a' andThen 'b').parse("ab").get === ('a','b'))
    // with strings
    assert(("a" andThen string("b")).parse("ab").get === ("a","b"))
    assert((string("a") andThen "b").parse("ab").get === ("a","b"))
    assert((string("ab") andThen string("cd")).parse("abcd").get === ("ab","cd"))
    // empty strings
    assert((string(" ") andThen string(" ")).parse("  ").get === (" "," "))
    // multiple andThen
    assert(('a' andThen 'b' andThen 'c' andThen 'd').parse("abcd").get === ((('a','b'),'c'),'d'))
    assert(("a" andThen string("b") andThen "c" andThen "d").parse("abcd").get === ((("a","b"),"c"),"d"))
    assert(('a' andThen ('b' orElse 'c') andThen 'd').parse("abd").get === (('a','b'),'d'))
    assert(('a' andThen ('b' orElse 'c') andThen 'd').parse("acd").get === (('a','c'),'d'))
    assert(((string("aa") andThen string("bb") andThen string("cc"))).parse("aabbcc").get === (("aa","bb"),"cc"))
  }
  // Failure
  "andThen Test Cases" should "throw an exception if either p1 or p2 fails" in {
    intercept[java.lang.Exception] {
      ('a' andThen 'b').parse("ba").get
      ('a' andThen 'b').parse("aa").get
      ('a' andThen 'b').parse("ac").get
      ('a' andThen 'b').parse("cd").get

      (string("a") andThen string("b")).parse("ba").get
      (string("a") andThen string("b")).parse("aa").get
      (string("a") andThen string("b")).parse("ac").get
      (string("a") andThen string("b")).parse("cd").get
      (string("ab") andThen string("cd")).parse("abce").get
      (string("ab") andThen string("cd")).parse("abed").get
      (string("ab") andThen string("cd")).parse("aecd").get
      (string("ab") andThen string("cd")).parse("ebcd").get
      (string("ab") andThen string("cd")).parse("ab").get
      (string("ab") andThen string("cd")).parse("cd").get
      (string("ab") andThen string("cd")).parse("").get
      (string(" ") andThen string("")).parse(" ").get
    }
  }

  // orElse-andThen
  // Success
  "orElse-andThen combined Test Cases" should "give the desired results" in {
    assert((('a' andThen 'b') orElse ('c' andThen 'd')).parse("ab").get === ('a','b'))
    assert((('a' andThen 'b') orElse ('c' andThen 'd')).parse("cd").get === ('c','d'))
    assert((('a' andThen 'b') orElse ('a' andThen 'a')).parse("ab").get === ('a','b'))
    assert((('a' andThen 'a') orElse ('a' andThen 'b')).parse("aa").get === ('a','a'))
    // complex combinations
    assert(((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get === ("cd","ab"))
  }
  // Failure
  "orElse-andThen combined Test Cases" should "throw an exception" in {
    intercept[java.lang.Exception] {
      (('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa").get
      // different order of evaluation
      ((('a' andThen 'b') orElse 'a') andThen 'c').parse("aac").get
      (('a' andThen 'b') orElse ('a' andThen 'c')).parse("aac").get
      ((string("cd") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get
    }
  }

  // orElse-attempt
  // Success
  "orElse-attempt Test Cases" should "fail in an uncommitted state, and backtrack from the point till which input is consumed" in {
    assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a','b'))
    assert((attempt(string("cd") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get === ("cd","ab"))
  }
  // repeat
  // Success
  "repeat Test Cases" should "repeat the given regular expression" in {
    assert(repeat('a').parse("aaa").get === List('a','a','a'))
    assert(repeat("ab").parse("abababab").get === List("ab","ab","ab","ab"))
    // repeat and orElse
    assert(repeat('a' orElse 'b').parse("aaa").get === List('a','a','a'))
    assert(repeat('a' orElse 'b').parse("aba").get === List('a','b','a'))
    assert(repeat('a' orElse 'b').parse("bba").get === List('b','b','a'))
    // repeat and andThen
    assert(repeat('a' andThen 'b').parse("ababab").get === List(('a','b'),('a','b'),('a','b')))
    // with map
    assert((repeat('a') map (_.size)).parse("aaa").get === 3)
  }

  // repeatN and digit test cases
  // Success
  "repeatN Test Cases" should "repeat the regular expression N times" in {
    assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
    assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a','a','a'))
  }
  // Failure
  "repeatN Test Cases" should "should throw an exception when the repeatition value is not equal to what is passed" in {
    intercept[java.lang.Exception] {
      (digit flatMap (repeatN(_)('a'))).parse("4aaa").get
    }
  }
  // regex
  // Success
  "regex Test Cases" should "match the regular expressions" in {
    assert(((regex("[a-z]*".r)).parse("abcd")).get === "abcd")
    assert(((regex("[a-z]*".r)).parse("abcd")).get === "abcd")
    assert(((regex("[[a]*|[b]]*".r)).parse("aa")).get === "aa")
    assert(((regex("[[a]*|[b]]*".r)).parse("b")).get === "b")
    assert(((regex("[[a]*|[b]]*".r)).parse("baa")).get === "baa")
    assert(((regex("[a-zA-Z0-9]*".r)).parse("Thisisatestofregularexpression")).get === "Thisisatestofregularexpression")
    assert(characters.parse("thisisatestofcharacters").get === "thisisatestofcharacters")
  }
  // Failure
  "regex Test Cases" should "throw an exception if the input deos not match the regular expressions" in {
    intercept[java.lang.Exception] {
      (regex("[a-z]*".r)).parse("abc34d").get
      (regex("[a-z]*".r)).parse("abcd09").get
      (regex("[[a]*|[b]]*".r)).parse("aafd").get
      (regex("[[a]*|[b]]*".r)).parse("bas").get
      (regex("[[a]*|[b]]*".r)).parse("bacd").get
    }
  }

  // repeat-repeatN-regex test case
  "repeat-regex combination" should "repeat the regular expression" in {
    assert(repeat((regex("[a-z]".r))).parse("aaabbb").get === List("a","a","a","b","b","b"))
    assert(repeatN(4)((regex("[0-9]".r))).parse("1234").get === List("4","3","2","1")) // returns a 4 digit number
  }

  // label
  "label Test Cases" should "replace the error message with any user defined error message" in {
    assert((label("The character doesn't match at location 3")(string("abc")).parse("ab7cd") recover { case ex => ex.getMessage }).get === "The character doesn't match at location 3")
    assert((label("First parser failed in committed state")(('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa") recover { case ex => ex.getMessage }).get === "First parser failed in committed state")
  }
  // tag
  // println used instead of assert to show that the custom error message is appended in front of the default error message
  "tag Test Cases" should "append the user defined error message to the existing error message" in {
    println((tag("The character doesn't match at location 3")(string("abc")).parse("ab7cd") recover { case ex => ex.getMessage }).get)
    println((tag("First parser failed in committed state")(('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa") recover { case ex => ex.getMessage }).get)

  }

}
