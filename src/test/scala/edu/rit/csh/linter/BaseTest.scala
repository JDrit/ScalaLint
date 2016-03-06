package edu.rit.csh.linter

import edu.rit.csh.linter.parser.Literals
import org.scalatest.FunSuite

class BaseTest extends FunSuite {

  import TestUtils._

  test("hex digit") {
    parse("f", Literals.hexDigit, Integer.valueOf("f", 16))
    parse("A", Literals.hexDigit, Integer.valueOf("A", 16))
    parse("5", Literals.hexDigit, Integer.valueOf("5", 16))
    parseError("G", Literals.hexDigit)
  }

  test("unicode escape") {
    parse("\\uE05A", Literals.unicodeEscape, "\uE05A")
  }

  test("op char") {
    parse("+-", Literals.op, '+-)
    parseError("the", Literals.op)
  }

  test("variable id") {
    parse("testing", Literals.varId, 'testing)
    parse("testing_+", Literals.varId, 'testing_+)
    parseError("Testing", Literals.varId)
  }

  test("plain id") {
    parse("testing", Literals.plainId, 'testing)
    parse("Testing", Literals.plainId, 'Testing)
    parse("testing_+", Literals.plainId, 'testing_+)
    parseError("this is a test", Literals.plainId)
  }

  test("id") {
    parse("testing", Literals.id, 'testing)
    parse("Testing", Literals.id, 'Testing)
    parse("`\"string literal\"`", Literals.id, Symbol("string literal"))
  }

  test("char No Double Quote Or New line") {
    parse("d", Literals.charNoDoubleQuoteOrNewline, "d")
    parseError("\"", Literals.charNoDoubleQuoteOrNewline)
  }

  test("character escape sequence") {
    parse("\\n", Literals.charEscapeSeq, "\n")
    parse("\\r", Literals.charEscapeSeq, "\r")
    parse("\\\\", Literals.charEscapeSeq, "\\")
    parseError("\\h", Literals.charEscapeSeq)
  }
}
