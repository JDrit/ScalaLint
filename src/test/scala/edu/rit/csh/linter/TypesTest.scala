package edu.rit.csh.linter

import edu.rit.csh.linter.language.Types.TypeDesignator
import edu.rit.csh.linter.parser.Types
import org.scalatest.{Ignore, FunSuite}

@Ignore
class TypesTest extends FunSuite {

  import TestUtils._

  test("Class Qualifier") {
    parse("[Class]", Types.classQualifier, "Class")
    parse("[`\"testing\"`]", Types.classQualifier, "testing")
  }

  test("stable ID") {
    parse("stableId", Types.stableId, "stableId")
    parse("stable.this.id", Types.stableId, "stable.this.id")
    parse("id.super[M].id2", Types.stableId, "id.super[M].id2")
    parse("id.this.id2", Types.stableId, "id.this.id2")
    parse("id1.id2.id3", Types.stableId, "id1.id2.id3")
  }

  test("path") {
    parse("C.this", Types.path, "C.this")
    parse("p.x", Types.path, "p.x")
    parse("C.super.x", Types.path, "C.super.x")
  }

  test("type") {

  }

  test("Types") {

  }

  test("Type Arguments") {

  }

  test("Annotations") {

  }

  test("Annotated Types") {

  }

  test("Simple Type") {
    parse("id1.id2.id3", Types.simpleType, TypeDesignator(Symbol("id1.id2.id3")))
  }

  test("Compound Types") {

  }

  test("Infix Types") {

  }
}
