package edu.rit.csh.linter

import edu.rit.csh.linter.language.Types.{ParameterizedType, TypeProjection, SingletonType, TypeDesignator}
import edu.rit.csh.linter.parser.Types
import org.scalatest.FunSuite

class TypesTest extends FunSuite {

  import TestUtils._

  test("class qualifier") {
    parse("[Class]", Types.classQualifier, 'Class)
    parse("[`\"testing\"`]", Types.classQualifier, 'testing)
  }

  test("stable ID") {
    parse("stableId", Types.stableId, 'stableId)
    parse("stable.this.id", Types.stableId, Symbol("stable.this.id"))
    parse("id.super[M].id2", Types.stableId, Symbol("id.super[M].id2"))
    parse("id.this.id2", Types.stableId, Symbol("id.this.id2"))
    parse("id1.id2.id3", Types.stableId, Symbol("id1.id2.id3"))
  }

  test("path") {
    parse("C.this", Types.path, Symbol("C.this"))
    parse("p.x", Types.path, Symbol("p.x"))
    parse("C.super.x", Types.path, Symbol("C.super.x"))
    parse("C.super[M].x", Types.path, Symbol("C.super[M].x"))
  }

  test("type") {

  }

  test("types") {

  }

  test("type arguments") {

  }

  test("annotations") {

  }

  test("annotated types") {

  }

  test("singleton types") {
    parse("edu.rit.csh.foo.type", Types.simpleType, SingletonType(Symbol("edu.rit.csh.foo")))
    parse("this.type", Types.simpleType, SingletonType('this))
  }

  test("type projections") {
    parse("A#B", Types.simpleType, TypeProjection(TypeDesignator('A), 'B))
  }

  test("type designators") {
    parse("t", Types.simpleType, TypeDesignator('t))
    parse("Int", Types.simpleType, TypeDesignator('Int))
    parse("scala.Int", Types.simpleType, TypeDesignator(Symbol("scala.Int")))
  }

  test("parameterized types") {
    parse("TreeMap[I, String]", Types.simpleType, ParameterizedType(TypeDesignator('TreeMap), TypeDesignator('I), TypeDesignator('String)))
  }

  test("compound types") {

  }

  test("infix types") {

  }
}
