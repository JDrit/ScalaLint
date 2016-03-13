package edu.rit.csh.linter

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Declarations._
import edu.rit.csh.linter.language.Expressions.LiteralExpression
import edu.rit.csh.linter.language.Literals.IntegerLiteral
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

  test("value pattern definition") {

  }

  test("variable pattern definition") {

  }

  test("pattern variable definition") {

  }

  test("function definition") {

  }

  test("variant type paramters") {

  }

  test("type definition") {
    parse("type id = Symbol", definition, TypeDef('id, Seq.empty, TypeDesignator('Symbol)))
  }

  test("tmpl definition") {

  }

  test("function signature") {

  }

  test("function type parameter clause") {

  }

  test("parameter clause") {

  }

  test("parameters") {
    parse("input", param, RegularParameter('input))
    parse("@depreciated input", param, RegularParameter('input,
      annotations = Seq(Annotation(TypeDesignator('depreciated)))))
    parse("@depreciated @test testing", param, RegularParameter('testing,
      annotations = Seq(Annotation(TypeDesignator('depreciated)), Annotation(TypeDesignator('test)))))
    parse("input: Int", param, RegularParameter('input,
      typ = Some(RegularParamType(TypeDesignator('Int)))))
    parse("param = 5", param, RegularParameter('param,
      expr = Some(LiteralExpression(IntegerLiteral(5)))))
    parse("param: Int = 5", param, RegularParameter('param,
      typ = Some(RegularParamType(TypeDesignator('Int))),
      expr = Some(LiteralExpression(IntegerLiteral(5)))))
  }

  test("implicit parameters") {

  }

  test("parameter types") {
    parse("Integer", paramType, RegularParamType(TypeDesignator('Integer)))
    parse("=> (String, Integer)", paramType, ByNameParamType(TupleType(TypeDesignator('String), TypeDesignator('Integer))))
    parse("String*", paramType, RepeatedParamType(TypeDesignator('String)))
    parse("String *", paramType, RepeatedParamType(TypeDesignator('String)))
  }
}
