package edu.rit.csh.linter

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Declarations._
import edu.rit.csh.linter.language.Expressions.{DefaultExpression, TupleExpression, LiteralExpression}
import edu.rit.csh.linter.language.Literals.{FloatingLiteral, IntegerLiteral}
import edu.rit.csh.linter.language.Patterns.{ConstructorPattern, TuplePattern, BindingPattern, VariablePattern}
import edu.rit.csh.linter.language.Types._
import edu.rit.csh.linter.parser.Declarations._
import org.scalatest.FunSuite

class DeclarationsTest extends FunSuite {
  import TestUtils._

  test("value declaration") {
    parse("val five: Int", dcl, ValDcl(Seq('five), TypeDesignator('Int)))
    parse("val one, two : Int", dcl, ValDcl(Seq('one, 'two), TypeDesignator('Int)))
  }

  test("variable declaration") {
    parse("var str : String", dcl, VarDcl(Seq('str), TypeDesignator('String)))
  }

  test("function declaration") {
    parse("def x: T", dcl, FunDcl('x, Seq.empty, Seq.empty, TypeDesignator('T)))
    parse("def identity[T] (x: T): T", dcl, FunDcl('identity, Seq(TypeParam('T)),
      Seq(Seq(RegularParameter('x, typ = Some(RegularParamType(TypeDesignator('T)))))),
      TypeDesignator('T)))
    parse("def add (x: Int)(y: Int): Int", dcl, FunDcl('add, Seq.empty,
      Seq(Seq(RegularParameter('x, typ = Some(RegularParamType(TypeDesignator('Int))))),
        Seq(RegularParameter('y, typ = Some(RegularParamType(TypeDesignator('Int)))))),
      TypeDesignator('Int)))
  }

  test("type declaration") {
    parse("type T", dcl, TypeDcl('T))
    parse("type T >: Numberic", dcl, TypeDcl('T, lowerBound = Some(TypeDesignator('Numberic))))
    parse("type T <: Numberic", dcl, TypeDcl('T, upperBound = Some(TypeDesignator('Numberic))))
    parse("type T [S]", dcl, TypeDcl('T, typParams = Seq(VariantTypeParam(TypeParam('S)))))
    parse("type T [S] >: N <: A", dcl, TypeDcl('T, lowerBound = Some(TypeDesignator('N)),
      upperBound = Some(TypeDesignator('A)), typParams = Seq(VariantTypeParam(TypeParam('S)))))
  }

  test("pattern definition") {
    parse("x : T = 5", patDef, PatternDef(Seq(VariablePattern('x)), Some(TypeDesignator('T)),
      LiteralExpression(IntegerLiteral(5))))
    parse("x = 5", patDef, PatternDef(Seq(VariablePattern('x)),
      exp = LiteralExpression(IntegerLiteral(5))))
    parse("x @ (y, z) = (1, 2)", patDef, PatternDef(Seq(BindingPattern('x,
      TuplePattern(VariablePattern('y), VariablePattern('z)))), exp = TupleExpression(
      LiteralExpression(IntegerLiteral(1)), LiteralExpression(IntegerLiteral(2)))))
  }

  test("value pattern definition") {
    parse("val pi = 3.1415", patVarDef, ValDef(PatternDef(Seq(VariablePattern('pi)),
      None, LiteralExpression(FloatingLiteral(3.1415)))))
    parse("val pi: Double = 3.1415", patVarDef, ValDef(PatternDef(Seq(VariablePattern('pi)),
      Some(TypeDesignator('Double)), LiteralExpression(FloatingLiteral(3.1415)))))
    parse("val Some(x) = 2", patVarDef, ValDef(PatternDef(
      Seq(ConstructorPattern('Some, VariablePattern('x))), None,
      LiteralExpression(IntegerLiteral(2)))))
    parse("val x :: xs = 5", patVarDef, ValDef(PatternDef(
      Seq(ConstructorPattern('::, VariablePattern('x), VariablePattern('xs))), None,
      LiteralExpression(IntegerLiteral(5)))))
  }

  test("variable pattern definition") {
    parse("var x, y: Int = _", patVarDef, VarDef(PatternDef(Seq(VariablePattern('x),
      VariablePattern('y)), Some(TypeDesignator('Int)), DefaultExpression(TypeDesignator('Int)))))
    parse("val pi = 3.1415", patVarDef, ValDef(PatternDef(Seq(VariablePattern('pi)),
      None, LiteralExpression(FloatingLiteral(3.1415)))))
    parse("val pi: Double = 3.1415", patVarDef, ValDef(PatternDef(Seq(VariablePattern('pi)),
      Some(TypeDesignator('Double)), LiteralExpression(FloatingLiteral(3.1415)))))
    parse("val Some(x) = 2", patVarDef, ValDef(PatternDef(
      Seq(ConstructorPattern('Some, VariablePattern('x))), None,
      LiteralExpression(IntegerLiteral(2)))))
    parse("val x :: xs = 5", patVarDef, ValDef(PatternDef(
      Seq(ConstructorPattern('::, VariablePattern('x), VariablePattern('xs))), None,
      LiteralExpression(IntegerLiteral(5)))))
  }

  test("function definition") {
    //parse("def one(x: Int): Int = 1", definition, )
    fail("not yet implemented")
  }

  test("type definition") {
    parse("type id = Symbol", definition, TypeDef('id, Seq.empty, TypeDesignator('Symbol)))
  }

  test("parameters") {
    parse("input", param, RegularParameter('input))
    parse("@depreciated input", param, RegularParameter('input,
      annotations = Seq(Annotation(TypeDesignator('depreciated)))))
    parse("@depreciated @test testing", param, RegularParameter('testing,
      annotations = Seq(Annotation(TypeDesignator('depreciated)),
        Annotation(TypeDesignator('test)))))
    parse("input: Int", param, RegularParameter('input,
      typ = Some(RegularParamType(TypeDesignator('Int)))))
    parse("param = 5", param, RegularParameter('param,
      expr = Some(LiteralExpression(IntegerLiteral(5)))))
    parse("param: Int = 5", param, RegularParameter('param,
      typ = Some(RegularParamType(TypeDesignator('Int))),
      expr = Some(LiteralExpression(IntegerLiteral(5)))))
  }

  test("implicit parameters") {
    parse("(implicit x: Int)", paramClauses, Seq(Seq(ImplicitParameter('x,
      typ = Some(RegularParamType(TypeDesignator('Int)))))))
    parse("(x: String)(implicit x1: Int)", paramClauses, Seq(Seq(RegularParameter('x,
      typ = Some(RegularParamType(TypeDesignator('String))))),
      Seq(ImplicitParameter('x1, typ = Some(RegularParamType(TypeDesignator('Int)))))))
  }

  test("parameter types") {
    parse("Integer", paramType, RegularParamType(TypeDesignator('Integer)))
    parse("=> (String, Integer)", paramType, ByNameParamType(TupleType(TypeDesignator('String),
      TypeDesignator('Integer))))
    parse("String*", paramType, RepeatedParamType(TypeDesignator('String)))
    parse("String *", paramType, RepeatedParamType(TypeDesignator('String)))
  }
}
