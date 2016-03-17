package edu.rit.csh.linter

import _root_.fastparse.core.Parsed.{Failure, Success}
import _root_.fastparse.core.{ParseError, Parser}
import edu.rit.csh.scalaLint.parser.{ScalaParser, ScalaLexer}
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import org.scalatest.FunSuite

import edu.rit.csh.linter.language.Literals._

object TestUtils extends FunSuite {

  def parse[T](str: String, parser: Parser[T], answer: T): Unit = {
    parser.parse(str) match {
      case Success(value, index) => assert(value === answer) ; assert(index === str.length)
      case f : Failure => fail(s"failed to parse `$str`, ${f.msg}")
    }
  }

  def parseError[T](str: String, parser: Parser[T]): Unit = {
    val result = parser.parse(str)
    result match {
      case Success(value, index) => assert(index !== str.length)
      case f: Failure => intercept[ParseError] {
        result.get
      }
    }
  }

  def parse2Error[T](str: String, f: ScalaParser => T): Unit = {
    val in = new ANTLRInputStream(str)
    val lexer = new ScalaLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ScalaParser(tokens)
    assert(f(parser) === null)
  }

  def parse2[T](str: String, expected: T, f: ScalaParser => T): Unit = {
    val in = new ANTLRInputStream(str)
    val lexer = new ScalaLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ScalaParser(tokens)
    assert(f(parser) === expected)
  }
}
