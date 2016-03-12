package edu.rit.csh.linter

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Declarations._
import edu.rit.csh.linter.language.Types._
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


  test("annotated types") {
    parse("String", Types.annotType, TypeDesignator('String))
    parse("String @local", Types.annotType, AnnotatedType(TypeDesignator('String), Annotation(TypeDesignator('local))))
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
    parse("TreeMap[I, String]", Types.simpleType, ParameterizedType(TypeDesignator('TreeMap),
      TypeDesignator('I), TypeDesignator('String)))
    parse("TreeMap[List[I], Int]", Types.simpleType, ParameterizedType(TypeDesignator('TreeMap),
      ParameterizedType(TypeDesignator('List), TypeDesignator('I)), TypeDesignator('Int)))
    parseError("TreeMap[I String]", Types.simpleType)
  }

  test("tuple types") {
    parse("(String, Int)", Types.simpleType, TupleType(TypeDesignator('String), TypeDesignator('Int)))
    parse("(String)", Types.simpleType, TypeDesignator('String))
    parseError("()", Types.simpleType)
  }

  test("compound types") {
    parse("String", Types.compoundType, TypeDesignator('String))
    parse("Cloneable with Resetable", Types.compoundType, CompoundType(Seq(TypeDesignator('Cloneable),
      TypeDesignator('Resetable)), Seq.empty))
    parse("Cloneable with Resetable with Showable", Types.compoundType, CompoundType(Seq(
      TypeDesignator('Cloneable), TypeDesignator('Resetable), TypeDesignator('Showable)), Seq.empty))
  }

  test("compound type with refinement") {
    parse("{ val callsign: String }", Types.compoundType, CompoundType(Seq.empty, Seq(ValDcl(
      Seq('callsign), TypeDesignator('String)))))
    parse("{ val callsign: String; def fly(height: Int): Unit }", Types.compoundType, CompoundType(Seq.empty,
      Seq(ValDcl(Seq('callsign), TypeDesignator('String)), FunDcl('fly, Seq.empty,
        Seq(Seq(RegularParameter(Seq.empty, 'height, Some(RegularParamType(TypeDesignator('Int))), None))), TypeDesignator('Unit)))))
  }

  test("infix types") {
    parse("String Pair Int", Types.infixType, InfixType(TypeDesignator('String), 'Pair, TypeDesignator('Int)))
    parse("String", Types.infixType, TypeDesignator('String))
  }

  test("function types") {
    parse("String", Types.typ, TypeDesignator('String))
    parse("String Pair Int", Types.typ, InfixType(TypeDesignator('String), 'Pair, TypeDesignator('Int)))
    parse("Int => Int", Types.typ, FunctionType(TypeDesignator('Int), TypeDesignator('Int)))
    parse("(Int) => Int", Types.typ, FunctionType(TypeDesignator('Int), TypeDesignator('Int)))
    parse("(Int, String) => Int", Types.typ, FunctionType(TupleType(TypeDesignator('Int), TypeDesignator('String)), TypeDesignator('Int)))
    parse("(Int, String) => Int => String", Types.typ, FunctionType(TupleType(TypeDesignator('Int), TypeDesignator('String)), FunctionType(TypeDesignator('Int), TypeDesignator('String))))
  }

  test("existential types") {
    parse("X forSome { type X }", Types.typ, ExistentialType(TypeDesignator('X), TypeDcl('X)))
    parse("VirtualMachine[A] forSome { type A }", Types.typ, ExistentialType(ParameterizedType(TypeDesignator('VirtualMachine), TypeDesignator('A)), TypeDcl('A)))
  }
}
