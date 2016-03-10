package edu.rit.csh.linter

import edu.rit.csh.linter.language.Literals.{FloatingLiteral, IntegerLiteral, StringLiteral}
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
    parse("\"testing 1 2 3\"", Patterns.simplePattern, LiteralPattern(StringLiteral("testing 1 2 3")))
    parse("-52", Patterns.simplePattern, LiteralPattern(IntegerLiteral(-52)))
  }

  test("stable ID pattern") {
    parse("this.id", Patterns.simplePattern, StableIdPattern(Symbol("this.id")))
    parse("StableID", Patterns.simplePattern, StableIdPattern('StableID))
  }

  test("tuple pattern") {
    parse("(x)", Patterns.tuplePattern, TuplePattern(VariablePattern('x)))
    parse("(x,y)", Patterns.tuplePattern, TuplePattern(VariablePattern('x), VariablePattern('y)))
    parse("(x,5)", Patterns.tuplePattern, TuplePattern(VariablePattern('x), LiteralPattern(IntegerLiteral(5))))
  }

  test("tuple pattern with spaces") {
    parse("( x )", Patterns.tuplePattern, TuplePattern(VariablePattern('x)))
    parse("( x , \"hi from JD\" )", Patterns.tuplePattern, TuplePattern(VariablePattern('x), LiteralPattern(StringLiteral("hi from JD"))))
    parse("( x , 5 )", Patterns.tuplePattern, TuplePattern(VariablePattern('x), LiteralPattern(IntegerLiteral(5))))
  }

  test("sequence pattern") {
    parse("StableID (_*)", Patterns.simplePattern, SequencePattern('StableID, Seq.empty, None))
    parse("Id(x@_*)", Patterns.simplePattern, SequencePattern('Id, Seq.empty, Some('x)))
    parse("Names(first, last, _*)", Patterns.sequencePattern, SequencePattern('Names, Seq(VariablePattern('first), VariablePattern('last)), None))
    parse("Names(first, last, rest@_*)", Patterns.sequencePattern, SequencePattern('Names, Seq(VariablePattern('first), VariablePattern('last)), Some('rest)))
    parse("Name2(x, _*)", Patterns.sequencePattern, SequencePattern('Name2, Seq(VariablePattern('x)), None))
    parseError("Error(one _*)", Patterns.simplePattern)
  }

  test("constructor pattern") {
    parse("Tuple2(x1, x2)", Patterns.simplePattern, ConstructorPattern('Tuple2, VariablePattern('x1), VariablePattern('x2)))
    parse("Tuple2(x1, _)", Patterns.simplePattern, ConstructorPattern('Tuple2, VariablePattern('x1), VariablePattern('_)))

  }
  test("typed patterns") {
    // parse("ex: IOException", Patterns.pattern, TypedPattern('ex, ))
  }

  test("binding patterns") {
    parse("t @ (x1, x2)", Patterns.pattern, BindingPattern('t, TuplePattern(VariablePattern('x1), VariablePattern('x2))))
  }

  test("infix operation patterns") {
    parse("1 :: 2 :: 3", Patterns.pattern3, ConstructorPattern('::, LiteralPattern(IntegerLiteral(1)), ConstructorPattern('::, LiteralPattern(IntegerLiteral(2)), LiteralPattern(IntegerLiteral(3)))))
  }

  test("alternative patterns") {
    parse("2 | 3", Patterns.pattern, AlternativePattern(LiteralPattern(IntegerLiteral(2)), LiteralPattern(IntegerLiteral(3))))
    parse("\"str\"", Patterns.pattern, LiteralPattern(StringLiteral("str")))
    parseError("5 || 6", Patterns.pattern)
  }

  test("pattern repeat") {
    parse("(\"string\", 52), 4.5f", Patterns.patterns, Seq(
      TuplePattern(LiteralPattern(StringLiteral("string")), LiteralPattern(IntegerLiteral(52))),
      LiteralPattern(FloatingLiteral(4.5))))
  }

}
