package edu.rit.csh.linter

import edu.rit.csh.linter.language.Literals.{IntegerLiteral, StringLiteral}
import edu.rit.csh.linter.language.Patterns.{TuplePattern, StableIdPattern, LiteralPattern, VariablePattern}
import edu.rit.csh.linter.parser.Patterns
import org.scalatest.FunSuite

class PatternsTest extends FunSuite {

  import TestUtils._

  test("variable pattern") {
    parse("_", Patterns.simplePattern, VariablePattern('_))
    parse("x", Patterns.simplePattern, VariablePattern('x))
    parseError("XError", Patterns.variablePattern)
  }

  test("literal pattern") {
    parse("\"testing\"", Patterns.simplePattern, LiteralPattern(StringLiteral("testing")))
    parse("-52", Patterns.simplePattern, LiteralPattern(IntegerLiteral(-52)))
  }

  test("stable ID pattern") {
    parse("this.id", Patterns.simplePattern, StableIdPattern(Symbol("this.id")))
  }

  test("tuple pattern") {
    parse("(x, y)", Patterns.simplePattern, TuplePattern(Seq(VariablePattern('x), VariablePattern('y))))
  }

  test("sequence pattern") {

  }

  test("Constructor pattern") {

  }

  test("pattern 1") {

  }

  test("pattern 2") {

  }

  test("pattern 3") {

  }

  test("pattern") {

  }

  test("patterns") {

  }
}
