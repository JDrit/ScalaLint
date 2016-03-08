package edu.rit.csh.linter

import edu.rit.csh.linter.language.Literals.{IntegerLiteral, StringLiteral}
import edu.rit.csh.linter.language.Patterns._
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
    parse("(x, 5)", Patterns.simplePattern, TuplePattern(Seq(VariablePattern('x), LiteralPattern(IntegerLiteral(5)))))
  }

  test("sequence pattern") {
    parse("StableId(x, xs@_*)", Patterns.simplePattern, SequencePattern('StableId, Seq(VariablePattern('x)), Some('xs)))
    parse("Name( _*)", Patterns.simplePattern, SequencePattern('Name, Seq.empty, None))
    parse("Name(one, two, 3, _*)", Patterns.simplePattern, SequencePattern('Name, Seq(VariablePattern('one), VariablePattern('two), VariablePattern('three)), None))
    parseError("Error(one _*)", Patterns.simplePattern)
  }

  test("Constructor pattern") {
    parse("Tuple2(x1, x2)", Patterns.simplePattern, ConstructorPattern('Tuple2, Seq(VariablePattern('x1), VariablePattern('x2))))

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
