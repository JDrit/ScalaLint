package edu.rit.csh.linter.language

import edu.rit.csh.linter.language.Annotations.Annotation
import edu.rit.csh.linter.language.Expressions.Expression
import edu.rit.csh.linter.language.Types.Typ

object Declarations {

  object Dependency extends Enumeration {
    val Covariant, Contravariant = Value
  }

  abstract class Declaration

  // A value declaration val x: T introduces x as a name of a value of type T.
  case class ValDcl(ids: Seq[Symbol], typ: Typ) extends Declaration

  case class VarDcl(ids: Seq[Symbol], typ: Typ) extends Declaration

  case class FunDcl(id: Symbol, typParams: Seq[TypeParam], params: Seq[Seq[Parameter]],
                    result: Typ) extends Declaration

  // A type declaration type t[tps] >: L <: U declares t to be an abstract type with lower bound
  // type L and upper bound type U. If the type parameter clause [tps] is omitted, t abstracts
  // over a first-order type, otherwise t stands for a type constructor that accepts type arguments
  // as described by the type parameter clause. If a type declaration appears as a member
  // declaration of a type, implementations of the type may implement t with any type T for
  // which L<:T<:U. It is a compile-time error if L does not conform to U. Either or both bounds
  // may be omitted. If the lower bound L is absent, the bottom type scala.Nothing is assumed. If
  // the upper bound U is absent, the top type scala.Any is assumed.
  case class TypeDcl(id: Symbol, typParams: Seq[VariantTypeParam] = Seq.empty, lowerBound: Option[Typ] = None,
                     upperBound: Option[Typ] = None) extends Declaration

  case class VariantTypeParam(annotations: Seq[Annotation], variance: Option[Dependency.Value],
                              param: TypeParam) extends Declaration

  // declares a new type, this is done like "type id = Symbol"
  case class TypeDef(id: Symbol, typeParams: Seq[VariantTypeParam], typ: Typ) extends Declaration

  type TypeParamClause = Seq[VariantTypeParam]

  abstract class Parameter(name: Symbol, annotations: Seq[Annotation], typ: Option[ParamType],
                       expr: Option[Expression]) extends Declaration

  case class RegularParameter(name: Symbol,
                              annotations: Seq[Annotation] = Seq.empty,
                              typ: Option[ParamType] = None,
                              expr: Option[Expression] = None)
    extends Parameter(name, annotations, typ, expr)

  case class ImplicitParameter(name: Symbol,
                               annotations: Seq[Annotation] = Seq.empty,
                               typ: Option[ParamType] = None,
                               expr: Option[Expression] = None)
    extends Parameter(name, annotations, typ, expr)

  // The most general form of a first-order type parameter is @a1…@an ± t >: L <: U. Here, L,
  // and U are lower and upper bounds that constrain possible type arguments for the parameter.
  // It is a compile-time error if L does not conform to U. ± is a variance, i.e. an optional
  // prefix of either +, or -. One or more annotations may precede the type parameter.
  case class TypeParam(name: Symbol,
                       params: Option[TypeParamClause] = None,
                       lowerBound: Option[Typ] = None,
                       upperBound: Option[Typ] = None,
                       contextBound: Option[Typ] = None)

  abstract class ParamType(typ: Typ) extends Typ

  case class RegularParamType(typ: Typ) extends ParamType(typ)

  // The type of a value parameter may be prefixed by =>, e.g. x: => T. The type of such a
  // parameter is then the parameterless method type => T. This indicates that the corresponding
  // argument is not evaluated at the point of function application, but instead is evaluated at
  // each use within the function. That is, the argument is evaluated using call-by-name.
  case class ByNameParamType(typ: Typ) extends ParamType(typ)

  // The last value parameter of a parameter section may be suffixed by '*', e.g. (..., x:T*).
  // The type of such a repeated parameter inside the method is then the sequence type scala.Seq[T].
  // Methods with repeated parameters T* take a variable number of arguments of type T. That is,
  // if a method m with type (p1:T1,…,pn:Tn,ps:S*)U is applied to arguments (e1,…,ek) where k≥n,
  // then m is taken in that application to have type (p1:T1,…,pn:Tn,ps:S,…,ps′S)U, with k−n
  // occurrences of type S where any parameter names beyond ps are fresh
  case class RepeatedParamType(typ: Typ) extends ParamType(typ)
}
