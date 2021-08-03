@author: Fatima Mushtaq
@date: 3/31/2018

## Parser Combinator Project

The goal of the project is to implement a parser combinator
library. 
* The parser combinator library is fully implemented with
  support for parsing from a `String` input.

* The library provides an implementation to a JSON parser
  (see [json.JAshutosh MahajanSON](src/main/scala/json/JSON.scala)) that
  converts a string in the JSON data exchange format into an abstract
  syntax tree representation of that
  data. See [here](https://www.json.org/) for a description of the
  JSON syntax.

* It further implement a simple command line calculator that parses arithmetic
  expressions over floating point numbers and evaluates them. The
  calculator evaluates the expression on-the-fly during parsing.

## To do Stretch Goals

* Extend the library with functionality for parsing from other input
  sources such as files. Extend your JSON parser appropriately to take
  advantage of the new functionality.

* Implement a simple command line calculator that parses arithmetic
  expressions over floating point numbers and evaluates them. The
  calculator should be implemented as a `Parser[Double]` that
  evaluates the expression on-the-fly during parsing.

* Generalize the parser to work on inputs other than character
  sequences. That is, your parser should work with abstract tokens
  rather than characters. These tokens can represent sequences of
  characters in a preprocessed input sequence. E.g. tokens may
  represent keywords, numbers, identifiers, string literals, etc. This
  way, you can split a parser for a complex language into two parts: a
  *tokenizer* (aka lexer) that parses the input character sequence to
  produce a token sequence as output, and the actual parser that
  parses the token sequence to produce, e.g., an abstract syntax
  tree. This is how complex parsers are typically organized.


