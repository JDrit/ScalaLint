package edu.rit.csh.linter

import edu.rit.csh.linter.language.Literals._
import edu.rit.csh.linter.parser.Literals
import org.scalatest.FunSuite

class LiteralsTest extends FunSuite {

  import TestUtils._

  def parseLiteral[T](str: String, expected: Literal[T]): Unit = parse2(str, expected, _.literal().value)
  def parseLiteralError[T](str: String): Unit = parse2Error(str, _.literal().value)

  test("integer literal") {
    parseLiteral("0", IntegerLiteral(0))
    parseLiteral("1", IntegerLiteral(1))
    parseLiteral("-1", IntegerLiteral(-1))
    parseLiteral("555", IntegerLiteral(555))
    parseLiteral("0x5", IntegerLiteral(0x5))
    parseLiteral("0x54", IntegerLiteral(0x54))
    parseLiteral("-0x54", IntegerLiteral(-0x54))
    parseLiteralError("01")
  }
/*
  test("floating literal") {
    parse("2.5", Literals.literal, FloatingLiteral(2.5))
    parse("-2.5", Literals.literal, FloatingLiteral(-2.5))
    parse("2.5f", Literals.literal, FloatingLiteral(2.5))
    parse("2.5F", Literals.literal, FloatingLiteral(2.5))
    parse("2.5d", Literals.literal, FloatingLiteral(2.5))
    parse("2.5D", Literals.literal, FloatingLiteral(2.5))
    parse("0.5", Literals.literal, FloatingLiteral(0.5))
    parse(".5e10", Literals.literal, FloatingLiteral(0.5e10))
    parse("-.5e10", Literals.literal, FloatingLiteral(-0.5e10))
    parse("0.5e-10", Literals.literal, FloatingLiteral(0.5e-10))
    parse("52e10f", Literals.literal, FloatingLiteral(52e10))
  }

  test("boolean literal") {
    parse("true", Literals.literal, BooleanLiteral(true))
    parse("false", Literals.literal, BooleanLiteral(false))
    parseError("False", Literals.literal)
  }

  test("character literal") {
    parse("'h'", Literals.literal, CharacterLiteral('h'))
    parse("'\\n'", Literals.literal, CharacterLiteral('\n'))
    parseError("''", Literals.literal)
  }

  test("string literal") {
    parse("\"some string\"", Literals.literal, StringLiteral("some string"))
    parse("\"\"", Literals.literal, StringLiteral(""))
    parse("\"\\n\"", Literals.literal, StringLiteral("\n"))
  }

  test("symbol literal") {
    parse("'id", Literals.literal, SymbolLiteral('id))
    parseError("id", Literals.symbolLiteral)
  }

  test("null literal") {
    parse("null", Literals.literal, NullLiteral())
  }

  */
}
