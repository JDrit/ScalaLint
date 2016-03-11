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
  /**
    * [S, T]
    * [@specialized T, U]
    * [Ex <: Throwable]
    * [A <: Comparable[B], B <: A]
    * [A, B >: A, C >: A <: B]
    * [M[X], N[X]]
    * [M[_], N[_]] // equivalent to previous clause
    * [M[X <: Bound[X]], Bound[_]]
    * [M[+X] <: Iterable[X]]
    */

  test("value declaration") {

  }

  test("variable declaration") {

  }

  test("function declaration") {

  }

  test("type declaration") {

  }

  test("value pattern definition") {

  }

  test("variable pattern definition") {

  }

  test("pattern variable definition") {

  }

  test("function definition") {

  }

  test("type definition") {

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
    parse("input", param, Parameter(Seq.empty, 'input, None, None))
    parse("@depreciated input", param, Parameter(Seq(Annotation(TypeDesignator('depreciated))),
      'input, None, None))
    parse("@depreciated @test testing", param, Parameter(
      Seq(Annotation(TypeDesignator('depreciated)), Annotation(TypeDesignator('test))), 'testing,
      None, None))
    parse("input: Int", param, Parameter(Seq.empty, 'input,
      Some(RegularParamType(TypeDesignator('Int))), None))
    parse("param = 5", param, Parameter(Seq.empty, 'param, None,
      Some(LiteralExpression(IntegerLiteral(5)))))
    parse("param: Int = 5", param, Parameter(Seq.empty, 'param,
      Some(RegularParamType(TypeDesignator('Int))),
      Some(LiteralExpression(IntegerLiteral(5))))))
  }

  test("parameter types") {
    parse("Integer", paramType, RegularParamType(TypeDesignator('Integer)))
    parse("=> (String, Integer)", paramType, ByNameParamType(TupleType(TypeDesignator('String), TypeDesignator('Integer))))
    parse("String*", paramType, RepeatedParamType(TypeDesignator('String)))
    parse("String *", paramType, RepeatedParamType(TypeDesignator('String)))
  }
}
