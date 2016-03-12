package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Declarations.Declaration

/**
  * Created by jd on 3/4/16.
  */
object Types {

  abstract class Typ

  // A singleton type is of the form p.type, where p is a path pointing to a value expected
  // to conform to scala.AnyRef. The type denotes the set of values consisting of null and
  // the value denoted by p.
  case class SingletonType(path: Symbol) extends Typ {
    override def toString(): String = s"${path.toString().substring(1)}.type"
  }

  // A type projection T#x references the type member named x of type T.
  case class TypeProjection(typ: Typ, id: Symbol) extends Typ

  // A type designator refers to a named value type. It can be simple or qualified.
  // All such type designators are shorthands for type projections.
  case class TypeDesignator(stableId: Symbol) extends Typ

  // A parameterized type T[T1,…,Tn] consists of a type designator T and type parameters T1,…,Tn
  // where n≥1. T must refer to a type constructor which takes n type parameters a1,…,an.
  case class ParameterizedType(typ: Typ, args: Typ*) extends Typ

  // A tuple type (T1,…,Tn) is an alias for the class scala.Tuplen[T1, … , Tn], where n≥2.
  case class TupleType(typs: Typ*) extends Typ

  // An annotated type T a1,…,an attaches annotations a1,…,an to the type T.
  case class AnnotatedType(typ: Typ, annotations: Annotation*) extends Typ

  // A compound type T1 with … with Tn{R} represents objects with members as given in the
  // component types T1,…,Tn and the refinement {R}. A refinement {R} contains declarations
  // and type definitions. If a declaration or definition overrides a declaration or definition
  // in one of the component types T1,…,Tn, the usual rules for overriding apply; otherwise the
  // declaration or definition is said to be “structural”
  case class CompoundType(tys: Seq[Typ], refinements: Seq[Declaration]) extends Typ

  // An infix type T1 op T2 consists of an infix operator op which gets applied to two
  // type operands T1 and T2. The type is equivalent to the type application op[T1,T2].
  // The infix operator op may be an arbitrary identifier.
  case class InfixType(left: Typ, operation: Symbol, right: Typ) extends Typ

  // An existential type has the form T forSome { Q } where Q is a sequence of type declarations.
  // Let t1[tps1]>:L1<:U1,…,tn[tpsn]>:Ln<:Un be the types declared in Q (any of the type parameter
  // sections [ tpsi ] might be missing). The scope of each type ti includes the type T and the
  // existential clause Q. The type variables ti are said to be bound in the type T forSome { Q }.
  // Type variables which occur in a type T but which are not bound in T are said to be free in T.
  case class ExistentialType(typ: Typ, typDeclarations: Declaration*) extends Typ

  // The type (T1,…,Tn)⇒U represents the set of function values that take arguments of types
  // T1,…,Tn and yield results of type U. In the case of exactly one argument type T⇒U is a
  // shorthand for (T)⇒U. An argument type of the form ⇒T represents a call-by-name parameter of
  // type T. Function types associate to the right, e.g. S⇒T⇒U is the same as S⇒(T⇒U)
  case class FunctionType(argument: Typ, result: Typ) extends Typ
}
