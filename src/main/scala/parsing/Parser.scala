package parsing

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
import java.util.regex.Pattern

class Location(inputValue: String, position: Int, endOfInput: Boolean) {
  val inputString: String = inputValue
  var currentPosition: Int = position
  var inputParsed: Boolean = endOfInput

 def matchRegex(r: Regex): Option[(String, Location)] = {

    if(!this.inputParsed) {
      var skipSpace:Boolean = true;  // Keeping Parser Space Insensitive

      while(skipSpace && !this.inputParsed && this.inputString.substring(this.currentPosition).startsWith(" ")){
        r.findPrefixOf(" ") match { case Some(space) => skipSpace = false case None => skipSpace=true }
        if(skipSpace) { this.currentPosition = this.currentPosition+1; this.inputParsed = this.currentPosition >= inputString.length; }
      }

      r.findPrefixOf(this.inputString.substring(this.currentPosition)) match {
        case Some(prefix) => {
          Some(prefix, new Location(inputString, this.currentPosition+prefix.length, this.currentPosition+prefix.length >= inputString.length));
        }
        case None => {  None }
      }
    }
    else None
  }

  override def toString = s"Location($inputString, $currentPosition, $inputParsed)"

}

object Location {
  implicit def fromString(s: String): Location = {
    new Location(inputValue = s, position = 0, endOfInput = s.isEmpty)
  }
  implicit def fromChar(c: Char): Location = {
    fromString(c.toString)
  }
}

private[parsing] sealed trait ParseState[+A] {
  def committed: Boolean
  def uncommit: ParseState[A]  // To uncommit a parser state i.e. setting the committed: Boolean to false
  def commit: ParseState[A]    // To commit a parser state i.e. setting the committed: Boolean to true

}
case class ParseStateSuccess[A](result: A, location: Location, override val committed: Boolean) extends ParseState[A] {
  override def uncommit: ParseState[A] = copy(committed = false)
  override def commit: ParseState[A] = copy(committed = true)
}
case class ParseStateFailure[A](errorMessage: String, location: Location, override val committed: Boolean) extends Exception with ParseState[Nothing] {
  override def uncommit: ParseState[Nothing] = copy(committed = false)
  override def commit: ParseState[Nothing] = copy(committed = true)
}

trait Parser[+A] {
  self =>
  private var stateFlag = false

  def apply(location: Location): ParseState[A]

  def parse(location: Location): Try[A] = {

    this.apply(location) match {
      case ParseStateSuccess(result, nextLocation, committed) =>
        if(nextLocation.inputParsed) {
          Success(result)
        } else {
          Failure(new Exception("Failure: Unable to parse due to garbage value at the end of input from position:" + (nextLocation.currentPosition + 1) + ""))
        }
      case ex@ParseStateFailure(errorMessage, errorLocation, committed) => Failure(new Exception(ex.errorMessage))
    }

  }

  def orElse[B >: A](p: => Parser[B]): Parser[B] = new Parser[B] {

    override def apply(loc: Location): ParseState[B] = {
      self.apply(loc) match {
        case ParseStateSuccess(result, location, committed) => ParseStateSuccess(result, location, committed)
        case st@ParseStateFailure(errorMessage, errorLocation, errorCommitted) => {
          if(errorCommitted)
            st
          else p.apply(loc) match {
            case ParseStateSuccess(result, location, committed) => ParseStateSuccess(result, location, committed)
            case ParseStateFailure(errorMessage, errorLocation, committed) => ParseStateFailure(errorMessage, errorLocation, committed)
          }
        }
      }
    }

  }

  def andThen[B](p: => Parser[B]): Parser[(A, B)] =
    for {
      a <- this
      b <- p
    } yield (a,b)

  def map[B](f: A => B): Parser[B] = new Parser[B] {

    override def apply(location: Location): ParseState[B] = {
      self.apply(location) match {
        case ParseStateSuccess(result, nextLocation, committed) => {
          ParseStateSuccess(f(result), nextLocation, committed)
        }
        case ParseStateFailure(errorMessage, errorLocation, committed) => {
          ParseStateFailure(errorMessage, errorLocation, committed)
        }
      }
    }

  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = new Parser[B] {

    override def apply(location: Location): ParseState[B] = {
      self.apply(location) match {
        case ParseStateSuccess(result, nextLocation, committed) => {
          self.stateFlag = committed
          val result_st = (f(result)(nextLocation))
          result_st.commit //Committing the state of returning object to true for AndThen (either is true)
        }
        case ParseStateFailure(errorMessage, errorLocation, committed) => {
          self.stateFlag = committed
          ParseStateFailure(errorMessage, errorLocation, committed)
        }
      }
    }

  }

  // Defining Aliases for andThen(Parse[A]), andThen(Parse[B]), andThen and OrElse
  def ~>[B](p: Parser[B]): Parser[B] = (this andThen p) map (_._2)

  def <~[B](p: Parser[B]): Parser[A] = (this andThen p) map (_._1)

  def ~[B](p: => Parser[B]): Parser[(A, B)] = this andThen p

  def |[B >: A](p: => Parser[B]): Parser[B] = this orElse p

}

object Parser {

  def succeed[A](a: A): Parser[A] = new Parser[A] {
    override def apply(location: Location): ParseState[A] = ParseStateSuccess(a, location, false)
  }

  def repeat[A](p: Parser[A]): Parser[List[A]] = new Parser[List[A]] {
    override def apply(location: Location): ParseState[List[A]] = attempt(p andThen(repeat(p)) map(a => (a._2).++:(List(a._1))) orElse(succeed(Nil))).apply(location)
  }

  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] =
    if(n == 0) {
      succeed(Nil)
    } else {
      p andThen(repeatN(n - 1)(p)) map(a => (List(a._1).++:(a._2)))
    }

  def attempt[A](p: Parser[A]): Parser[A] = new Parser[A] {
    override def apply(location: Location): ParseState[A] = {
      p.apply(location).uncommit
    }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = new Parser[A] {

    override def apply(location: Location): ParseState[A] = p.apply(location) match {
      case ParseStateSuccess(res, nextLocation, committed) => ParseStateSuccess(res, nextLocation, committed)
      case ParseStateFailure(errorMessage, errorLocation, committed) => ParseStateFailure(msg, location, committed)
    }

  }

  def tag[A](msg: String)(p: Parser[A]): Parser[A] = new Parser[A] {

    override def apply(location: Location): ParseState[A] = p.apply(location) match {
      case ParseStateSuccess(result, nextLocation, committed) => ParseStateSuccess(result, nextLocation, committed)
      // Appending message at the start of failure message
      case ParseStateFailure(errorMessage, errorLocation, committed) => ParseStateFailure(msg + " - " + errorMessage, location, committed)
    }

  }

  implicit def string(s: String): Parser[String] = regex(s.r)

  implicit def char(c: Char): Parser[Char] = new Parser[Char] {

    override def apply(location: Location): ParseState[Char] = {

      location.matchRegex(Pattern.quote(c.toString).r) match {
        case Some((prefix, nextLocation)) => ParseStateSuccess(c, nextLocation, true)
        case None => {
          if(location.inputParsed && "".equals(location.inputString.substring(location.currentPosition))) {
            //Case: Finished Parsing all input, unable to define parser, commit state = true
            ParseStateFailure("Unable to parse Input: \"" + location.inputString + "\" - Parser not defined for '" + location.inputString.substring(location.currentPosition - 1) + "' at position: " + (location.currentPosition), location, false)
          } else {
            //CHAR - Unable to define parser because of incorrect input sequence, commit state = false
            if(location.inputString.length == 1) {
              ParseStateFailure("Unable to Parse Input - Parser not defined for '" + location.inputString.substring(location.currentPosition) + "'", location, false)
            } else {
              ParseStateFailure("Unable to parse Input: \"" + location.inputString + "\" - Parser not defined for '" + location.inputString.substring(location.currentPosition) + "' at position: " + (location.currentPosition), location, false)
            }
          }
        }
      }
    }

  }

  implicit def regex(r: Regex): Parser[String] = {

    new Parser[String] {

      override def apply(location: Location): ParseState[String] = location.matchRegex(r) match {
        case Some((str, nextLocation)) => ParseStateSuccess(str, nextLocation, true);
        case None => {
          if(location.inputParsed && "".equals(location.inputString.substring(location.currentPosition))) {
            // Case: Finished Parsing all input, unable to define parser, commit state = true
            ParseStateFailure("Parser not defined for " + r, location, false)
          } else {
            // Case ("REGEX - Unable to define parser because of incorrect input sequence, commit state = false")
            ParseStateFailure("Unable to parse Input - Parser not defined for: \"" + location.inputString.substring(location.currentPosition) + "\"" + " at position:" + (location.currentPosition+1), location, committed = false)
          }
        }
      }
    }

  }

  def digit: Parser[Int] = {
    "[0-9]".r map (_.toInt)
  }

  def digits: Parser[Int] =
    (digit andThen repeat(digit)) map {
      case (d, ds) => (d :: ds) reduce (_ * 10 + _)
    }

  def characters: Parser[String] = {
    """([^"\\]|\\"|\\b|\\f|\\n|\\r|\\t|\\/|\\\\|\\u[0-9a-f]{4})*""".r
  }

  def numbers: Parser[Double] = {
    """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+\-]?\d+)?""".r map (_.toString.toDouble)
  }

}
