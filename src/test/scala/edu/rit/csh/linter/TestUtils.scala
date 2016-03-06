package edu.rit.csh.linter

import _root_.fastparse.core.Parsed.{Failure, Success}
import _root_.fastparse.core.{ParseError, Parser}
import org.scalatest.FunSuite

object TestUtils extends FunSuite {

  def parse[T](str: String, parser: Parser[T], answer: T): Unit = {
    parser.parse(str) match {
      case Success(value, index) => assert(value === answer) ; assert(index === str.length)
      case f : Failure => fail(s"failed to parse ($str), ${f.msg}")
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
}
