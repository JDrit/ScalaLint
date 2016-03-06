package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Expressions.Expression

/**
  * Created by jd on 3/4/16.
  */
object Types {

  abstract class Typ

  abstract class SimpleType extends Typ

  // A singleton type is of the form p.type, where p is a path pointing to a value expected
  // to conform to scala.AnyRef. The type denotes the set of values consisting of null and
  // the value denoted by p.
  case class SingletonType(path: String, typ: Typ) extends SimpleType

  // A type projection T#x references the type member named x of type T.
  case class TypeProjection(typ: SimpleType, id: String) extends SimpleType

  // A type designator refers to a named value type. It can be simple or qualified.
  // All such type designators are shorthands for type projections.
  case class TypeDesignator(stableId: String) extends SimpleType

  // A parameterized type T[T1,…,Tn] consists of a type designator T and type parameters T1,…,Tn
  // where n≥1. T must refer to a type constructor which takes n type parameters a1,…,an.
  case class ParameterizedType(typ: SimpleType, args: Seq[Typ]) extends SimpleType

  // A tuple type (T1,…,Tn) is an alias for the class scala.Tuplen[T1, … , Tn], where n≥2.
  case class TupleType(typs: Seq[Typ]) extends SimpleType

  // An annotated type T a1,…,an attaches annotations a1,…,an to the type T.
  case class AnnotatedType(typ: SimpleType, annotations: Seq[Annotation]) extends Typ

  // A compound type T1 with … with Tn{R} represents objects with members as given in the
  // component types T1,…,Tn and the refinement {R}. A refinement {R} contains declarations
  // and type definitions. If a declaration or definition overrides a declaration or definition
  // in one of the component types T1,…,Tn, the usual rules for overriding apply; otherwise the
  // declaration or definition is said to be “structural”
  case class CompoundType(tys: Seq[AnnotatedType]) extends Typ

  // An infix type T1 op T2 consists of an infix operator op which gets applied to two
  // type operands T1 and T2. The type is equivalent to the type application op[T1,T2].
  // The infix operator op may be an arbitrary identifier.
  case class InfixType(left: Typ, right: Option[(String, Typ)]) extends Typ

  case class FunctionType(types: Seq[Typ], result: Typ) extends Typ
}
